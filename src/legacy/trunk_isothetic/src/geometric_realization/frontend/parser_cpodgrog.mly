%{

  open BuiltInWithoutParsers

  let display_ar ar = 
    if false 
    then Printf.printf "raw = %s\nhl = %s\n" 
      (Message.red (ODA.OverInteger.Sh.string_of ar)) 
      (Message.red (ODA.OverInteger.HL.string_of ar))

 let empty = ODA.OverInteger.Sh.empty

 let additional_vertex = ref []

 let hidden_vertex = ref []

 let add_vertex x = additional_vertex := x::!additional_vertex

 let add_hidden_vertex x = hidden_vertex := x::!hidden_vertex

 let a_dgrog description = 
   let aux = DGROG.draw_edges_list description
   in
   List.iter
     (
       fun x -> 
	 aux.DGROG.vertices <- DGROG.Enriched.V.add x aux.DGROG.vertices ;
	 DGROG.G.add_vertex aux.DGROG.edges x
     )
     !additional_vertex ;
   additional_vertex := [] ;
   List.iter
     (
       fun x -> DGROG.G.add_vertex aux.DGROG.edges x
     )
     !hidden_vertex ;
   hidden_vertex := [] ;
   aux

 let draw src ?(label=E.String.default) ?(area=Expression.ODA.Variable "") tgt ?accu () =
   let _,ar,b = 
     match
       Sheet_solver_oda.calculation
	 (Some (if src=tgt then Circle else CompactHalfLine),area) 
	 (Expression.add "" (Sheet_solver_oda.Area(None,empty,false)) Expression.empty)
(*Dirty Hack: there is no contructor for the empty set so it is
  assumed that the content of the variable named "" (the empty
  string) is always empty*)
     with
       | Sheet_solver_oda.Area x -> x
       | _ -> failwith "Type Mismatch [Parser_cpodgrog]"
   in
   match accu with
     | Some accu -> (src,label,ar,tgt,b)::accu
     | None      -> [src,label,ar,tgt,b]

 let brick b = Area.of_brick (Brick.of_array b)

 let union b accu = 	  
   Area.union 
     (Area.of_brick (Brick.of_array b)) accu
%}

%token <string> IDENT ERROR
/*%token <BuiltInWithoutParsers.ODA.OverInteger.Sh.t * bool> AREA */
%token <Expression.ODA.operator> AREA
%token MINUS SEMICOLON COLON END
%token UNION TIMES
%token LPAR RPAR LCURL RCURL LSQBR RSQBR
%left UNION
%left TIMES
%start output
%type <BuiltInWithoutParsers.Area.t> output
%%

description:
| IDENT IDENT AREA IDENT SEMICOLON description {draw $1 ~label:$2    ~area:$3 $4 ~accu:$6 ()}
| IDENT IDENT      IDENT SEMICOLON description {draw $1 ~label:$2 $3             ~accu:$5 ()}
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

  brick:
| description                                  {[|a_dgrog $1|]}
| description TIMES brick                      {Array.append [|a_dgrog $1|] $3}
  ;

  area:
| brick {brick $1}
| brick UNION area {union $1 $3}
  ;

  output:
| area END {$1}
