%{

  open GraphOverInteger
  
  module M = Map.Make(String)
  
  let symbols = ref (M.empty)

  let graph = ref G.empty

%}

%token <string> ID
%token <int> INT
%token EQUAL OPAR CPAR OBRA CBRA OCURLY CCURLY INFTY
%token COLON SEMICOLON
%token MEET JOIN DIFFERENCE COMPLEMENT FULL EMPTY 
%token FUTURE_EXTENSION PAST_EXTENSION 
%token INTERIOR CLOSURE
%token FUTURE_CLOSURE PAST_CLOSURE
%token END
%token <string> ERROR
%start output
%type < GraphOverInteger.G.t * (GraphOverInteger.R.t Map.Make(String).t) > output
%%

  arrow_or_vertex:
| INT COLON INT INT {graph := G.add_arrow $3 $1 $4 !graph}
| INT               {graph := G.add_vertex $1 !graph}

  graph:
| arrow_or_vertex graph {}
| arrow_or_vertex       {}

  bounded:
| CBRA INT INT OBRA {I.bounded false $2 $3 false}
| OBRA INT INT CBRA {I.bounded true  $2 $3 true}
| CBRA INT INT CBRA {I.bounded false $2 $3 true}
| OBRA INT INT OBRA {I.bounded true  $2 $3 false}

  singleton:
| OCURLY INT CCURLY {I.atom $2}

  terminal:
| OBRA INT INFTY OBRA {I.terminal true $2}
| CBRA INT INFTY OBRA {I.terminal false $2}

  halfline_region:
| bounded halfline_region {$1 :: $2}
| singleton halfline_region {$1 :: $2}
| terminal halfline_region {$1 :: $2}
| bounded {[$1]}
| singleton {[$1]}
| terminal {[$1]}

  region:
| INT COLON halfline_region region {R.add_arrow $1 $3 $4}
| INT region {R.add_vertex $1 $2}
| INT COLON halfline_region {R.add_arrow $1 $3 R.empty}
| INT {R.add_vertex $1 R.empty}

  expression:
| ID {try M.find $1 !symbols with Not_found -> (Printf.printf "undefined %s" $1; M.iter (fun k _ -> Printf.printf "%s " k) !symbols; assert false)}
| OPAR region CPAR {$2}
| MEET expression expression {R.meet !graph $2 $3} 
| JOIN expression expression {R.join !graph $2 $3} 
| DIFFERENCE expression expression {R.difference !graph $2 $3}
| FUTURE_EXTENSION expression expression {R.future_extension !graph $2 $3}
| PAST_EXTENSION expression expression {R.past_extension !graph $2 $3}
| COMPLEMENT expression {R.complement !graph $2}
| INTERIOR expression {R.interior !graph $2}
| CLOSURE expression {R.closure !graph $2}
| FUTURE_CLOSURE expression {R.future_closure !graph $2}
| PAST_CLOSURE expression {R.past_closure !graph $2}

  declaration:
| ID EQUAL region
    {symbols := M.add $1 (R.zero_normalize !graph $3) !symbols}
| ID EQUAL expression
    {symbols := M.add $1 (R.zero_normalize !graph $3) !symbols}

  declarations:
| declaration declarations {}
| declaration  {}

  output:
| graph declarations END { !graph , !symbols }
| graph END { !graph , M.empty }
| END { G.empty , M.empty }
