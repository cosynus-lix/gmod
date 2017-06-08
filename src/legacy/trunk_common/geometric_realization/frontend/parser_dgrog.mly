%{

  open BuiltInWithoutParsers

  let empty = ODA.OverInteger.Sh.empty

  let display_ar ar =
    if false
    then Printf.printf "raw = %s\nhl = %s\n"
      (Message.red (BuiltInWithoutParsers.ODA.OverInteger.Sh.string_of ar))
      (Message.red (BuiltInWithoutParsers.ODA.OverInteger.HL.string_of ar))

(*Dirty Hack: there is no constructor for the empty set so it is assumed that the content of the variable named "" (the empty
string) is always empty*)

  let draw src ?(label=E.String.default) ?(area=Expression.ODA.Variable "") tgt ?accu () =
    let _,ar,b = (
      match
        Sheet_solver_oda.calculation
          (Some (if src=tgt then Circle else CompactHalfLine),area)
          (Expression.add "" (Sheet_solver_oda.Area (None,empty,false)) Expression.empty)
      with
        | Sheet_solver_oda.Area x -> x
        | _ -> failwith "Type Mismatch [Parser_dgrog]" ) in
    match accu with
      | Some accu -> (src,label,ar,tgt,b) :: accu
      | None      -> [src,label,ar,tgt,b]

  let empty = BuiltInWithoutParsers.ODA.OverInteger.Sh.empty

  let additional_vertex = ref []

  let hidden_vertex = ref []

  let add_vertex x = additional_vertex := x :: !additional_vertex

  let add_hidden_vertex x = hidden_vertex := x :: !hidden_vertex

%}

%token <string> IDENT ERROR
%token <Expression.ODA.operator> AREA
%token MINUS SEMICOLON COLON END
%token EQUAL
%token INTERSECTION UNION DIFFERENCE CLOSURE INTERIOR FUTURE PAST
%token LPAR RPAR LCURL RCURL LSQBR RSQBR
%left EQUAL
%left FUTURE PAST
%left DIFFERENCE
%left UNION
%left INTERSECTION
%start output
%type <BuiltInWithoutParsers.DGROG.t> output
%%

description:
| IDENT IDENT AREA IDENT SEMICOLON description {draw $1 ~label:$2    ~area:$3 $4 ~accu:$6 ()}
| IDENT IDENT      IDENT SEMICOLON description {draw $1 ~label:$2             $3 ~accu:$5 ()}
| IDENT       AREA IDENT SEMICOLON description {draw $1              ~area:$2 $3 ~accu:$5 ()}
| IDENT            IDENT SEMICOLON description {draw $1                       $2 ~accu:$4 ()}
| IDENT                  SEMICOLON description {add_vertex $1;$3}
| MINUS IDENT            SEMICOLON description {add_hidden_vertex $2;$4}
| IDENT IDENT AREA IDENT SEMICOLON             {draw $1 ~label:$2    ~area:$3 $4          ()}
| IDENT IDENT AREA IDENT                       {draw $1 ~label:$2    ~area:$3 $4          ()}
| IDENT IDENT      IDENT SEMICOLON             {draw $1 ~label:$2             $3          ()}
| IDENT IDENT      IDENT                       {draw $1 ~label:$2             $3          ()}
| IDENT       AREA IDENT SEMICOLON             {draw $1              ~area:$2 $3          ()}
| IDENT       AREA IDENT                       {draw $1              ~area:$2 $3          ()}
| IDENT            IDENT SEMICOLON             {draw $1                       $2          ()}
| IDENT            IDENT                       {draw $1                       $2          ()}
| MINUS IDENT                                  {add_hidden_vertex $2;[]}
| MINUS IDENT            SEMICOLON             {add_hidden_vertex $2;[]}
| IDENT                                        {add_vertex $1;[]}
| IDENT                  SEMICOLON             {add_vertex $1;[]}
  ;

  output:
| description END {
    let open BuiltInWithoutParsers.DGROG in
    let aux = draw_edges_list $1 in
    List.iter (fun x ->
      G.add_vertex aux.edges x)
      !hidden_vertex ;
    hidden_vertex := [] ;
    List.iter (fun x -> (
      G.add_vertex aux.edges x ;
      aux.vertices <- Enriched.V.add x aux.vertices) )
      !additional_vertex ;
    additional_vertex := [] ;
    aux}
