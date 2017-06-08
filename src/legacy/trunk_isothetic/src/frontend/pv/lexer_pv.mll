{
  (*open Type*)
  open Parser_pv
  open Lexing
(*
  let verbose = ref false
*)
  let no_parameter s = if !Parser_pv_flags.lexer_verbose then print_endline s
  let one_parameter s p = if !Parser_pv_flags.lexer_verbose then Printf.printf "%s %s\n" s p
}

let ident = ['_' 'a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

  rule entry_point = parse
    | [' ''\t''\r']+                                 {entry_point lexbuf}
    | '%'[^'\n']*                                    {entry_point lexbuf} (* comments *)
    | "\n"                                           {Lexing.new_line lexbuf; entry_point lexbuf}
    | ['0'-'9']+ as n                                {one_parameter "INT" n  ; INT (int_of_string n)}
    | "INIT:"|"Init:"|"init:"|"usrRoot:"|"#init"     {no_parameter "INIT" ; INIT}
    | "processes:"|"procs:"|"#process"|"#proc"       {no_parameter "PROCESS" ; PROCESS}
    | "("                                            {no_parameter "LPAR" ; LPAR}
    | ")"                                            {no_parameter "RPAR" ; RPAR}
    | "["                                            {no_parameter "LBRC" ; LBRC}
    | "]"                                            {no_parameter "RBRC" ; RBRC}
    | "int"                                          {no_parameter "TINT" ; TINT}
    | "bool"                                         {no_parameter "TBOOL" ; TBOOL}
    | "true"                                         {one_parameter "BOOL" "true" ; BOOL true}
    | "false"                                        {one_parameter "BOOL" "false" ; BOOL false}
    | "P"                                            {no_parameter "P" ; P} (*Prolaag – probeer te verlagen – lock a resource*)
    | "V"                                            {no_parameter "V" ; V} (*Verhogen – unlock a resource*)
    | "S" | "send"                                   {no_parameter "S" ; S} (*Send – a message on a channel*)
    | "R" | "receive"                                {no_parameter "R" ; R} (*Receive – a message on a channel*)
    | "M" | "monitor"                                {no_parameter "M" ; M} (*Monitoring – lock a resource and wait for some signal*)
    | "N" | "notify"                                 {no_parameter "N" ; N} (*Notify – some monitor*)
    | "A" | "all" | "notify_all"                     {no_parameter "A" ; A} (*Notify All – monitors*)
    | "F" | "fork"                                   {no_parameter "F" ; F} (*Fork – create a process, currently ignored*)
    | "C" | "call"                                   {no_parameter "C" ; C} (*Call – a process*)
    | "E" | "exec"                                   {no_parameter "E" ; E} (*Execute – same as Call (?)*)
    | "W" | "wait"                                   {no_parameter "W" ; W} (*Wait – behind a bareer*)
    | "@"                                            {no_parameter "T" ; T} (*Load – the content of some variable*)
    | "+"                                            {no_parameter "PLUS" ; PLUS}
    | "-"                                            {no_parameter "MINUS" ; MINUS}
    | "*"                                            {no_parameter "TIMES" ; TIMES}
    | "/"                                            {no_parameter "DIVIDE" ; DIVIDE}
    | "abs"                                          {no_parameter "ABS" ; ABS} (*absolute*)
    | "<=" | "⩽"                                     {no_parameter "LEQ" ; LEQ} (*less or equal test*)
    | ">=" | "⩾"                                     {no_parameter "GEQ" ; GEQ} (*less or equal test*)
    | "<"                                            {no_parameter "LS" ; LS}
    | ">"                                            {no_parameter "GS" ; GS}
    | "=="                                           {no_parameter "EQ" ; EQ} (*equal test*)
    | "<>" | "≠"                                     {no_parameter "NEQ" ; NEQ} (*not equal test*)
    | "%" | "mod"                                    {no_parameter "MOD" ; MODULO} (*modulo*)
    | "not" | "¬"                                    {no_parameter "NOT" ; NOT} (*boolean negation*)
    | "#void" | "ø" | "Ø"                            {no_parameter "VOID" ; VOID} (*empty process*)
    | ":"                                            {no_parameter "DDOT" ; DDOT}
    | ";"                                            {no_parameter "SC" ; SC}
    | "."                                            {no_parameter "DOT" ; DOT}
    | ","                                            {no_parameter "COMMA" ; COMMA}
    | "="                                            {no_parameter "DEFINE" ; DEFINE}
    | ":="                                           {no_parameter "DDEFINE" ; DDEFINE}
    | "|" | "⋁"                                      {no_parameter "OR" ; OR} (*Boolean Or / Parallel composition*)
    | "&" | "⋀"                                      {no_parameter "AND" ; AND} (*Boolean And*)
    | "?"                                            {no_parameter "QM" ; QM} (*Question Mark*)
    | ("#incl"|"#include")[' ']([^'\n']* as f)       {one_parameter "INCLUDE" f ; INCLUDE f}
    | "#mutex_default" | "#mtxd" | "#mutex" | "#mtx" {no_parameter "MUTEX_DEFAULT" ; MUTEX_DEFAULT}
    | "#mutex_recursive" | "#mtxr"                   {no_parameter "MUTEX_REC" ; MUTEX_REC}
    | "#mutex_normal" | "#mtxn"                      {no_parameter "MUTEX_NORMAL" ; MUTEX_NORMAL}
    | "#mutex_errorcheck" | "#mtxec"                 {no_parameter "MUTEX_EC" ; MUTEX_EC}
    | "#semaphore" | "#sem"                          {no_parameter "SEMAPHORE" ; SEMAPHORE}
    | "#semaphore_quantitative" | "#semq"            {no_parameter "SEMAPHORE_QUANT" ; SEMAPHORE_QUANT}
    | "#monitor" | "#mntr"                           {no_parameter "MONITOR" ; MONITOR}
    | "#synchronization" | "#syn"                    {no_parameter "SYNCHRONIZATION" ; SYNCHRONIZATION}
    | "#fifo" | "#queue"                             {no_parameter "FIFO" ; FIFO}
    | "#lifo" | "#stack"                             {no_parameter "LIFO" ; LIFO}
    | ident as ident                                 {one_parameter "IDENT" ident ; IDENT ident}
    | "{"([^'}']* as msg)"}"                         {one_parameter "MSG" msg ; MSG msg}
    | eof                                            {EOF}
    | _ as invalid_character                         {no_parameter (Printf.sprintf "INVALID_CHARACTER %c" invalid_character) ; INVALID_CHARACTER}
