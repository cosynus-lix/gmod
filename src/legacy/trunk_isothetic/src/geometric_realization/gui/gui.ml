(*graphical user interface*)

module Callback =
struct
(*TODO*)
end


let main_window_accel_group = 
  let ag = GtkData.AccelGroup.create () in
  let () = GtkData.AccelGroup.connect 
    ~key:GdkKeysyms._q
    ~modi:[`MOD1]
    ~callback:(GMain.quit)
    ag
  in ag

let main_window = GWindow.window
  ~title:"Directed Geometric Realisation of Directed Graphs"
  ~position:`CENTER
  ~height:603 ~width:1072 () 

let () = main_window#add_accel_group main_window_accel_group 

let menubar_n_textviews = GPack.vbox ~packing:main_window#add ()

let top_toolbar = GMenu.menu_bar ~height:24 ~show:true ~packing:menubar_n_textviews#pack ()

let file_menu = GMenu.menu_item ~use_mnemonic:true ~label:"_File" ~packing:top_toolbar#add ~show:true ()

let file_submenu = GMenu.menu ~packing:file_menu#set_submenu  ()

let save_item = GMenu.menu_item ~label:"Save" ~packing:file_submenu#add  ()

let save_as_item = GMenu.menu_item ~label:"Save as" ~packing:file_submenu#add  ()

let load_item = GMenu.menu_item ~label:"Load" ~packing:file_submenu#add  ()

let quit_item = GMenu.menu_item ~label:"Quit" ~packing:file_submenu#add  ()

let edit_menu = GMenu.menu_item ~use_mnemonic:true ~label:"_Edit" ~packing:top_toolbar#add ~show:true ()

let display_menu = GMenu.menu_item ~use_mnemonic:true ~label:"_Display" ~packing:top_toolbar#add ~show:true ()

let compute_menu = GMenu.menu_item ~use_mnemonic:true ~label:"_Compute" ~packing:top_toolbar#add ~show:true ()

let request_n_result = GPack.hbox ~spacing:2 ~packing:menubar_n_textviews#add ()

let left_buffer  = GText.buffer ~text:"To be computed\n" ()

let right_buffer = GText.buffer ~text:"Result\n" ()

let left_text  = GText.view ~width:536 ~height:603 ~buffer:left_buffer ~packing:(request_n_result#add) ()

let right_text = GText.view ~width:536 ~height:603 ~buffer:right_buffer ~packing:(request_n_result#add) ()

let signals =
  let _ = quit_item#event#connect#button_press ~callback:(fun _ -> GMain.quit ();true)
  in 
  let _ = load_item#event#connect#button_press
    ~callback:
    (
      fun _ -> 
	Printf.printf "%s was clicked\n%!" Common.Terminal.(color Blue ~bold:true "\"load\"");
	let dialog = GWindow.file_chooser_dialog ~action:`OPEN 
	  ~title:"load file" 
	  ~show:true () 
	in
	let () = dialog#add_select_button "open" `OPEN in
	let _ = dialog#connect#file_activated
	  ~callback:
	  (
	    fun () ->
	      let filename = match dialog#filename with 
		| Some filename -> filename 
		| None -> failwith "Unexpected [Gui]"
	      in
	      Printf.printf "Open file %s\n%!" 
		Common.Terminal.(color Blue ~bold:true filename)
	      ;
	      (
		try
		  (
		    let chan = open_in filename in
		    let answer = ref "" in
		    try
		      while true 
		      do
			answer := !answer^"\n"^(input_line chan)
		      done
		    with 
		      | End_of_file ->
			(
			  (left_text#buffer#set_text !answer) 
			  ;
			  close_in chan
			)
		  )
		with
		  | Sys_error s -> (Printf.printf "Sys_error %s\n%!" s)
	      )
	      ;
	      Printf.printf "%s was clicked\n%!" Common.Terminal.(color Blue ~bold:true "\"Compute\"");
	      (
		right_text#buffer#set_text
		  (
		    Sheet_solver_cpodgrog.string_of
		      (
			Sheet_solver_cpodgrog.from_string
			  (left_text#buffer#get_text ()) (*The source of the computation*)
		      )
		  )
	      )
	      ;dialog#destroy ()
	  )
	in
	ignore dialog;
	true
    )
  in
  let _ =
    save_item#event#connect#button_press 
      ~callback:(fun _ -> Printf.printf "%s was clicked\n%!" Common.Terminal.(color Blue ~bold:true "\"Save\"");true)
  in 
  let _ =
    save_as_item#event#connect#button_press 
      ~callback:(fun _ -> Printf.printf "%s was clicked\n%!" Common.Terminal.(color Blue ~bold:true "\"Save as\"");true)
  in 
  let _ = 
    compute_menu#event#connect#button_press
      ~callback:
      (
	fun _ -> 
	  Printf.printf "%s was clicked\n%!" Common.Terminal.(color Blue ~bold:true "\"Compute\"");
	  (
	    right_text#buffer#set_text
	      (
		Sheet_solver_cpodgrog.string_of
		  (
		    Sheet_solver_cpodgrog.from_string
		      (left_text#buffer#get_text ()) (*The source of the computation*)
		  )
	      )
	  )
	  ;
	  true
      )
  in
  ()

let go () =
  Printf.printf "%s\n%s\n%!" 
    Common.Terminal.(color Blue ~bold:true "Initialisation of GTK (locale description below)") 
    (GMain.init ()) (*returns a string containing the locale*)
  ; 
  ignore (main_window#connect#destroy ~callback:GMain.quit)
  ;
  if !Parameter.Gui.fullscreen then main_window#maximize ()
  ;
  main_window#show ()
  ;
  print_endline Common.Terminal.(color Blue ~bold:true "Run the main loop")
  ;
  GMain.main ()
