let alpha = 0.4

type sv =
  <
    highlight : (float array * float array) list -> unit;
    set_intervals : (float array * float array) list -> unit;
  >

class state_viewer ?packing () =
  let area = GlGtk.area [`RGBA; `DOUBLEBUFFER] ?packing () in
  let quad_of_interval_3d (p1,p2) =
    [
      [|
        p1.(0),p1.(1),p1.(2);
        p1.(0),p2.(1),p1.(2);
        p2.(0),p2.(1),p1.(2);
        p2.(0),p1.(1),p1.(2);
      |];
      [|
        p1.(0),p1.(1),p2.(2);
        p2.(0),p1.(1),p2.(2);
        p2.(0),p2.(1),p2.(2);
        p1.(0),p2.(1),p2.(2);
      |];
      [|
        p1.(0),p1.(1),p1.(2);
        p2.(0),p1.(1),p1.(2);
        p2.(0),p1.(1),p2.(2);
        p1.(0),p1.(1),p2.(2);
      |];
      [|
        p1.(0),p2.(1),p1.(2);
        p2.(0),p2.(1),p1.(2);
        p2.(0),p2.(1),p2.(2);
        p1.(0),p2.(1),p2.(2);
      |];
      [|
        p1.(0),p1.(1),p1.(2);
        p1.(0),p2.(1),p1.(2);
        p1.(0),p2.(1),p2.(2);
        p1.(0),p1.(1),p2.(2);
      |];
      [|
        p2.(0),p1.(1),p1.(2);
        p2.(0),p2.(1),p1.(2);
        p2.(0),p2.(1),p2.(2);
        p2.(0),p1.(1),p2.(2);
      |]
    ]
  in
object (self)
  val mutable dimension = 3

  val mutable quads3 = []
  val mutable quads3_highlight = []

  val mutable angle = 0.

  method init () =
    let light_ambient = 0., 0., 0., 0.
    and light_diffuse = 0.8, 0.8, 0.8, 1.
    and light_specular = 0., 0., 0., 0.
    and light_position = 0.1, -0.2, 1.2, 1.
    in
    GlLight.light ~num:0 (`ambient light_ambient);
    GlLight.light ~num:0 (`diffuse light_diffuse);
    GlLight.light ~num:0 (`specular light_specular);
    GlLight.light ~num:0 (`position light_position);

    GlFunc.depth_func `less;
    List.iter Gl.enable [`lighting; `light0; `depth_test; `color_material (*; `blend *)];
    GlDraw.shade_model `flat;
    GlFunc.blend_func `src_alpha `one;

  method reshape ~width ~height =
    GlDraw.viewport 0 0 width height;
    GlMat.mode `projection;
    GlMat.load_identity ();
    GlMat.ortho (-1., 2.) (-1., 2.) (-2., 2.)
    (* GluMat.perspective ~fovy:60.0 ~aspect:(float width /. float height) ~z:(1.0,20.0); *)
    (* GlMat.mode `modelview *)

  method display () =
    GlMat.push ();
    GlClear.clear [`color; `depth];
    (* Draw the axis. *)
    GlMat.rotate ~angle ~x:1. ~y:1. ~z:1. ();
    GlDraw.color (0.,0.,1.);
    GlDraw.begins `lines;
    GlDraw.vertex3 (0., 0., 0.);
    GlDraw.vertex3 (1., 0., 0.);
    GlDraw.vertex3 (0., 0., 0.);
    GlDraw.vertex3 (0., 1., 0.);
    GlDraw.vertex3 (0., 0., 0.);
    GlDraw.vertex3 (0., 0., 1.);
    GlDraw.ends ();

    (*
    GlDraw.color (0.,1.,1.);
    GlDraw.begins `lines;
    GlDraw.vertex3 (0., 0.5, 0.);
    GlDraw.vertex3 (1., 0.5, 0.);
    GlDraw.vertex3 (0., 0.5, 0.5);
    GlDraw.vertex3 (1., 0.5, 0.5);
    GlDraw.vertex3 (0., 0.5, 1.);
    GlDraw.vertex3 (1., 0.5, 1.);

    GlDraw.vertex3 (0.5, 0., 0.);
    GlDraw.vertex3 (0.5, 1., 0.);
    GlDraw.vertex3 (0.5, 0., 0.5);
    GlDraw.vertex3 (0.5, 1., 0.5);
    GlDraw.vertex3 (0.5, 0., 1.);
    GlDraw.vertex3 (0.5, 1., 1.);

    GlDraw.vertex3 (0., 0., 0.5);
    GlDraw.vertex3 (1., 0., 0.5);
    GlDraw.vertex3 (0., 0.5, 0.5);
    GlDraw.vertex3 (1., 0.5, 0.5);
    GlDraw.vertex3 (0., 1., 0.5);
    GlDraw.vertex3 (1., 1., 0.5);
    GlDraw.ends ();
    *)

    if dimension = 3 then
      (
        GlDraw.begins `quads;
        GlDraw.color ~alpha (1.,0.,0.);
        List.iter
          (fun q ->
            for i = 0 to 3 do
              GlDraw.vertex3 q.(i)
            done
          ) (List.filter (fun q -> not (List.mem q quads3_highlight)) quads3);
        GlDraw.color ~alpha (0.,1.,0.);
        List.iter
          (fun q ->
            for i = 0 to 3 do
              GlDraw.vertex3 q.(i)
            done
          ) quads3_highlight;
        GlDraw.ends ();
      );
    GlMat.pop ();
    Gl.flush ();
    area#swap_buffers ()

  method set_intervals i =
    if i = [] then
      dimension <- 0
    else
      dimension <- Array.length (fst (List.hd i));
    (* TODO: hide the window *)
    (*
      if dimension > 3 then
      area#obj#show false;
    *)
    quads3 <- [];
    quads3_highlight <- [];
    if dimension = 3 then
      List.iter (fun i -> quads3 <- (quad_of_interval_3d i)@quads3) i

  method highlight i =
    quads3_highlight <- [];
    if dimension = 3 then
      List.iter (fun i -> quads3_highlight <- (quad_of_interval_3d i)@quads3_highlight) i

  method rotate () =
    angle <- angle +. 2.;
    if angle >= 360. then angle <- angle -. 360.;
    if area#misc#visible then self#display ();
    true

  initializer
    ignore (area#connect#reshape ~callback:self#reshape);
    ignore (area#connect#realize ~callback:self#init);
    ignore (area#connect#display ~callback:self#display);
    ignore (GMain.Timeout.add ~ms:20 ~callback:self#rotate)
end

let state_viewer ?packing () = (new state_viewer ?packing () :> sv)

let () =
  Display_with_gtk2.state_viewer := state_viewer
