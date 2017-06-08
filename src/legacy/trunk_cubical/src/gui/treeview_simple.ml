type 'a tree = Node of string * 'a * ('a -> unit) * 'a tree list

class ['a] treeview ?tree ?packing () =
  let tv = GTree.view ~headers_visible:false ?packing () in
  let cols = new GTree.column_list in
  let column = cols#add Gobject.Data.string in
  let treestore = GTree.tree_store cols in
object (self)
  val mutable actions : (GTree.row_reference * ('a -> unit) * 'a) list = []

  method append ?parent l =
    match l with
      | (Node (s,d,f,ll))::t ->
          let row = treestore#append ?parent () in
            treestore#set ~row ~column s;
            actions <- (treestore#get_row_reference (treestore#get_path row), f, d)::actions;
            self#append ~parent:row ll;
            self#append ?parent t
      | [] -> ()

  initializer
    let col = GTree.view_column ~renderer:(GTree.cell_renderer_text [], ["text", column]) () in
      ignore (tv#append_column col);
      tv#set_model (Some (treestore#coerce));
      tv#selection#set_mode `SINGLE;
      ignore (tv#selection#connect#changed ~callback:self#on_select)

  method remove_children row =
    while treestore#iter_has_child row do
      let child = treestore#iter_children (Some row) in
        actions <-
        List.filter
          (fun (rr,_,_) ->
             GTree.Path.get_indices rr#path <> GTree.Path.get_indices (treestore#get_path child)
          ) actions;
        ignore (treestore#remove child)
    done

  method clear = treestore#clear ()

  method set t =
    self#clear;
    self#append t

  method private on_select () =
    let row = tv#selection#get_selected_rows in
      match row with
        | [] -> ()
        | row::_ ->
            let parent = treestore#get_iter row in
            let f = List.find (fun (rr,_,_) -> GTree.Path.get_indices rr#path = GTree.Path.get_indices row) actions in
            let (_,f,x) = f in
              f x;
              self#remove_children parent;
              tv#expand_row row
end
