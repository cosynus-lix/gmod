open Common

let flash_delay = 3000

(* Fake implementation. *)
let state_viewer ?(packing:(GObj.widget -> unit) option) () =
  object
    method highlight (i:(float array * float array) list) = ()
    method set_intervals (i:(float array * float array) list) = ()
  end

let state_viewer = ref state_viewer

let abstract_interpretation = ref (fun (_:Semantics.instr list) -> ([]:string list))

type tree = TVNode of
    (string                (* label *)
     * (unit -> tree list) (* on click: sets the children with the return value *)
     * tree list)

class virtual treeview_semantics ?packing () =
  let tv = GTree.view ~headers_visible:false ?packing () in
  let cols = new GTree.column_list in
  let column = cols#add Gobject.Data.string in
  let treestore = GTree.tree_store cols in
object (self)
  val mutable actions = []

  method virtual on_state : unit -> tree list
  method virtual on_forbidden : unit -> tree list
  method virtual on_deadlock_attractor : unit -> tree list
  method virtual on_unreachable : unit -> tree list

  method private append ?parent l =
    match l with
      | (TVNode (s,f,ll))::t ->
          let row = treestore#append ?parent () in
            treestore#set ~row ~column s;
            actions <- (treestore#get_row_reference (treestore#get_path row), f)::actions;
            self#append ~parent:row ll;
            self#append ?parent t
      | [] -> ()

  method private fill =
    self#append [
      TVNode ("State", self#on_state, []);
      TVNode ("Forbidden", self#on_forbidden, []);
      TVNode ("Deadlocks", self#on_deadlock_attractor, []);
      TVNode ("Unreachable", self#on_unreachable, []);
    ];
    self#semantics_select_by_name "Forbidden"

  initializer
    let col = GTree.view_column ~renderer:(GTree.cell_renderer_text [], ["text", column]) () in
      ignore (tv#append_column col);
      tv#set_model (Some (treestore#coerce));
      tv#selection#set_mode `SINGLE;
      ignore (tv#selection#connect#changed ~callback:self#on_select);
      self#fill;

  method private remove_children row =
    while treestore#iter_has_child row do
      let child = treestore#iter_children (Some row) in
        actions <-
        List.filter
          (fun rr ->
             GTree.Path.get_indices (fst rr)#path <> GTree.Path.get_indices (treestore#get_path child)
          ) actions;
        ignore (treestore#remove child)
    done

  method private on_select () =
    let row = tv#selection#get_selected_rows in
      match row with
        | [] -> ()
        | row::_ ->
            let parent = treestore#get_iter row in
            let f = List.find (fun rr -> GTree.Path.get_indices (fst rr)#path = GTree.Path.get_indices row) actions in
            let f = snd f in
            let childs = f () in
              self#remove_children parent;
              self#append ~parent childs;
              tv#expand_row row

  method semantics_update = self#on_select ()

  method semantics_select_by_name n =
    let row = ref None in
      treestore#foreach
        (fun _ r ->
           if treestore#get ~row:r ~column = n then
             (
               row := Some r;
               true
             )
           else
             false
        );
      let row =
        match !row with
          | Some row -> row
          | None -> assert false
      in
        tv#selection#select_iter row

  method semantics_clear =
    actions <- [];
    treestore#clear ();
    self#fill

end

class gui () =
  let w = new Gui2.window () in
  let status = w#statusbar#new_context "oplate" in
  let source_buffer = GText.buffer () in
  let sb_tag_highlight = source_buffer#create_tag [`BACKGROUND "yellow"] in
  let (* source_text *) _ = GText.view ~width:300 ~buffer:source_buffer ~packing:w#scrolled_source#add () in
  let notebook = GPack.notebook ~tab_pos:`TOP ~packing:w#hpaned#add () in

  (* Fill in the tabs. *)
  let semantics = new Gui2.semantics () in
  let (* nb_semantics *) _ = notebook#append_page ~tab_label:(GMisc.label ~text:"Semantics" ())#coerce semantics#semantics#coerce in
  let semantics_scrolled = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:semantics#semantics#add1 () in
  let state_viewer = !state_viewer ~packing:semantics#semantics#add2 () in
  let abstract_testing = new Gui2.abstract_testing () in
  let (* nb_abstract_testing *) _ = notebook#append_page ~tab_label:(GMisc.label ~text:"Abstract testing" ())#coerce abstract_testing#abstract_testing#coerce in
  let abstract_testing_tv = new Treeview_simple.treeview ~packing:abstract_testing#sw_maximal_traces#add () in
  let vars_col, vars_ts =
    let vars_tv = abstract_testing#treeview_variables in
    let cols = new GTree.column_list in
    let column = cols#add Gobject.Data.string in
    let treestore = GTree.tree_store cols in
    let col = GTree.view_column ~renderer:(GTree.cell_renderer_text [], ["text", column]) () in
    ignore (vars_tv#append_column col);
    vars_tv#set_model (Some (treestore#coerce));
    vars_tv#selection#set_mode `SINGLE;
    column, treestore
  in

  (* Fill the toolbar. *)
  let btn_openfile = GButton.tool_button ~stock:`OPEN ~packing:w#toolbar#insert () in
  let btn_savefile = GButton.tool_button ~stock:`SAVE ~packing:w#toolbar#insert () in
  let btn_execute = GButton.tool_button ~stock:`EXECUTE ~packing:w#toolbar#insert () in
object (self)
  inherit treeview_semantics ~packing:semantics_scrolled#add ()

  method clear =
    (* Interpreter.runpro := [||] ; *)
    (* Interpreter.init () ;
       Type.semantics := Type.empty_environment (); *)
    (* Geometric_model.init (); *)
    self#highlight [];
    self#highlight_viewer []

  method execute () =
    self#semantics_update;
    self#on_abstract_testing

  method highlight i =
    source_buffer#remove_tag
      sb_tag_highlight
      source_buffer#start_iter
      source_buffer#end_iter;
    List.iter
      (fun (x,y) ->
        let start = source_buffer#get_iter (`OFFSET x) in
        let stop = source_buffer#get_iter (`OFFSET y) in
        source_buffer#apply_tag sb_tag_highlight ~start ~stop
      ) i

  (* TODO: factorize with highlight *)
  method highlight_viewer i =
    state_viewer#highlight i

  method semantics =
    try
      Analyzer.semantics_from_string ~fmt:self#buffer_format self#source_code
    with
    | Parsing.Parse_error as e ->
      status#flash ~delay:flash_delay "Error: could not parse source code.";
      raise e

  method display_region f =
    self#clear;
    let a = self#semantics in
    let runpro = Interpreter.init_runpro a in
    Printf.printf "There are %i running processes\n" (Array.length runpro);
    Analyzer.run_down_resources a;
    let f = f a in
    let bb = Display.bounding_box_of_area f in
    let cubes = Type.AC.fold (fun c l -> c::l) f [] in
    let cubes = List.filter (fun c -> not (Type.C.is_empty c)) cubes in
    let n = ref (-1) in
    let drepr = Display.representation ~bb f in
    (* TODO: why do I have to do this? *)
    let drepr = if cubes = [] then [] else drepr in
    state_viewer#set_intervals drepr;
    List.map
      (fun c ->
        incr n;
        TVNode
          (
            Printf.sprintf "Cube %d: %s" !n (Type.C.string_of c),
            (fun () ->
              let i = Display.highlight_pv c runpro in
              state_viewer#set_intervals (Display.representation ~bb f);
              self#highlight i;
              (* TODO: function for this *)
              self#highlight_viewer (Display.representation ~bb (Type.AC.make [c]));
              []
            ),
            []
          )
      ) cubes

  method on_state () =
    self#display_region Type.Geometric_model.state
  method on_forbidden () =
    self#display_region Type.Geometric_model.forbidden
  method on_deadlock_attractor () =
    self#display_region Type.Geometric_model.deadlock_attractor
  method on_unreachable () =
    self#display_region Type.Geometric_model.unreachable

  method source_code =
    source_buffer#get_text ()

  method about () =
    let md =
      GWindow.about_dialog
        ~authors:["Emmanuel Haucourt"; "Samuel Mimram"; "Michel Hirschowitz"]
        ~comments:" alpha "
        ~copyright:"Copyright (C)"
        ~name:"oplate"
        ~version:"0.1"
        ~website:"http://cea.fr"
        ()
    in
    ignore (md#run ());
    md#destroy ()

  val mutable fname = ""

  method newfile () =
    fname <- "";
    source_buffer#set_text ""

  (** Simple detection of the format of the buffer. *)
  method buffer_format =
    if Filename.check_suffix fname ".c" || Filename.check_suffix fname ".C" then
      `C
    else
      `PV

  method loadfile f =
    fname <- f;
    let ic = open_in f in
    let s = ref "" in
    try
      while true do
        s := !s ^ (input_line ic) ^ "\n"
      done
    with
    | End_of_file ->
      close_in ic;
      source_buffer#set_text !s;
      self#semantics_clear

  method openfile () =
    let f =
      let fc = GWindow.file_chooser_dialog ~action:`OPEN ~show:true () in
      fc#add_button_stock `CANCEL `CANCEL;
      fc#add_select_button_stock `OPEN `OPEN;
      fc#add_filter (GFile.filter ~name:"Supported files" ~patterns:["*.pv";"*.c"] ());
      fc#add_filter (GFile.filter ~name:"PV files (*.pv)" ~patterns:["*.pv"] ());
      fc#add_filter (GFile.filter ~name:"C files (*.c)" ~patterns:["*.c"] ());
      fc#add_filter (GFile.filter ~name:"All files (*)" ~patterns:["*"] ());
      let f =
        match fc#run () with
        | `OPEN -> fc#filename
        | `DELETE_EVENT | `CANCEL -> None
      in
      fc#destroy ();
      f
    in
    (* let f = GToolbox.select_file ~title:"Choose file to open..." () in *)
    match f with
    | None -> ()
    | Some f ->
      self#loadfile f

  method savefile ?(saveas=false) () =
    let fname =
      if saveas || fname = "" then
        let fc = GWindow.file_chooser_dialog ~action:`SAVE ~show:true () in
        fc#add_button_stock `CANCEL `CANCEL;
        fc#add_select_button_stock `SAVE `SAVE;
        fc#add_filter (GFile.filter ~name:"Supported files" ~patterns:["*.pv";"*.c"] ());
        fc#add_filter (GFile.filter ~name:"PV files (*.pv)" ~patterns:["*.pv"] ());
        fc#add_filter (GFile.filter ~name:"C files (*.c)" ~patterns:["*.c"] ());
        fc#add_filter (GFile.filter ~name:"All files (*)" ~patterns:["*"] ());
        let f =
          match fc#run () with
          | `SAVE -> fc#filename
          | `DELETE_EVENT | `CANCEL -> None
        in
        fc#destroy ();
        f
      else
        Some fname
    in
    match fname with
    | Some fname ->
      let oc = open_out fname in
      output_string oc self#source_code;
      close_out oc
    | None -> ()

  val mutable at_instr = 0
  val mutable at_path = [||]

  method on_abstract_testing =
    let show_dipath p =
      at_path <- p;
      self#at_last ()
    in
    (* Compute the semantics. *)
    let a = self#semantics in
    (* Running processes. *)
    let runpro = Interpreter.init_runpro a in
    (* Maximal dipaths. *)
    let maxdip = Type.Geometric_model.maximal_dipaths a in
    let maxdip = Type.Co.fold (fun x l -> x::l) maxdip [] in
    (* Dimension = number of processes. *)
    let dim = Array.length runpro in
    (* Maximal length of a path (bounding box). *)
    let max_coord = Array.init dim (fun k -> Array.length runpro.(k).Semantics.code + 1) in
    let n = ref (-1) in
    let tree =
      List.map
        (fun p ->
          let text_pos code n =
            if n = 0 then
              fst (snd code.(0))
            else
              snd (snd code.(n-1))
          in
          let path =
	    (* Lift the path from geometry to the code. *)
            Path_extract.generic
              ~runpro
              (fun ?runpro k ip ->
                fst (get_some runpro).(k).Semantics.code.(ip.(k)-1),
                Array.to_list
                  (
                    Array.init dim
                      (fun k ->
                        let code = (get_some runpro).(k).Semantics.code in
			let ipk = ip.(k) in
                        (text_pos code 0,text_pos code ipk)
                      )
                  )
              )
              max_coord
              p
          in
          let path = Array.of_list path in
          Treeview_simple.Node
            (Printf.sprintf "Dipath %d" (incr n; !n),
             path,
             show_dipath,
             [])
        ) maxdip
    in
    abstract_testing_tv#set tree

  method at_set_variables v =
    vars_ts#clear ();
    List.iter
      (fun s ->
        let row = vars_ts#append () in
        vars_ts#set ~row ~column:vars_col s
      ) v

  method at_set n =
    let n = max n 0 in
    let n = min (Array.length at_path) n in
    at_instr <- n;
    abstract_testing#lbl_instr#set_text (Printf.sprintf "instr: %d" at_instr);
    if at_instr = 0 then
      (
        self#highlight [];
        self#at_set_variables []
      )
    else
      (
        self#highlight (snd at_path.(at_instr - 1));
        let av =
          !abstract_interpretation
            (List.map fst (Array.to_list (Array.sub at_path 0 n)))
        in
        self#at_set_variables av
      )

  method at_first () = self#at_set 0

  method at_last () = self#at_set (Array.length at_path)

  method at_incr () = self#at_set (at_instr + 1)

  method at_decr () = self#at_set (at_instr - 1)

  method on_notebook n =
    if n = 1 then
      self#on_abstract_testing

  initializer
    (* Connect everything. *)
    ignore (w#window#connect#destroy ~callback:GMain.Main.quit);
    ignore (w#menu_quit#connect#activate ~callback:GMain.Main.quit);
    ignore (w#menu_new#connect#activate ~callback:self#newfile);
    ignore (w#menu_open#connect#activate ~callback:self#openfile);
    ignore (w#menu_save#connect#activate ~callback:self#savefile);
    ignore (w#menu_saveas#connect#activate ~callback:(self#savefile ~saveas:true));
    ignore (w#menu_about#connect#activate ~callback:self#about);
    ignore (btn_openfile#connect#clicked ~callback:self#openfile);
    ignore (btn_savefile#connect#clicked ~callback:self#savefile);
    ignore (btn_execute#connect#clicked ~callback:self#execute);
    ignore (notebook#connect#switch_page ~callback:self#on_notebook);
    ignore (abstract_testing#btn_first#connect#clicked ~callback:self#at_first);
    ignore (abstract_testing#btn_back#connect#clicked ~callback:self#at_decr);
    ignore (abstract_testing#btn_forward#connect#clicked ~callback:self#at_incr);
    ignore (abstract_testing#btn_last#connect#clicked ~callback:self#at_last);
    (* Display the window. *)
    w#window#show ();
    (* Load the file on the commandline. *)
    (
      match !Globals.name_of_the_input_file with
      | Some f -> self#loadfile f
      | None -> ()
    )
end

let gui () =
  let _ = new gui () in
    GMain.Main.main ()

let () =
  Globals.have_gtk := true;
  Globals.display_with_gtk_gui := gui
