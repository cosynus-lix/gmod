{
  (*open Type*)
  open Parser_pv
  open Lexing
}

let ident = ['_' 'a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

  rule entry_point = parse
    | [' ''\t''\r']+                                 {entry_point lexbuf}
    | '%'[^'\n']*                                    {entry_point lexbuf} (* comments *)
    | "\n"                                           {Lexing.new_line lexbuf; entry_point lexbuf}
    | ['0'-'9']+ as n                                {INT (int_of_string n)}
    | "INIT:"|"Init:"|"init:"|"usrRoot:"             {INIT}
    | "processes:"|"procs:"                          {PROCS}
    | "("                                            {LPAR}
    | ")"                                            {RPAR}
    | "["                                            {LBRC}
    | "]"                                            {RBRC}
    | "int"                                          {TINT}
    | "bool"                                         {TBOOL}
    | "true"                                         {BOOL true}
    | "false"                                        {BOOL false}
    | "P"                                            {P} (*Prolaag – probeer te verlagen – lock a resource*)
    | "V"                                            {V} (*Verhogen – unlock a resource*)
    | "S" | "send"                                   {S} (*Send – a message on a channel*)
    | "R" | "receive"                                {R} (*Receive – a message on a channel*)
    | "M" | "monitor"                                {M} (*Monitoring – lock a resource and wait for some signal*)
    | "N" | "notify"                                 {N} (*Notify – some monitor*)
    | "A" | "all" | "notify_all"                     {A} (*Notify All – monitors*)
    | "F" | "fork"                                   {F} (*Fork – create a process, currently ignored*)
    | "C" | "call"                                   {C} (*Call – a process*)
    | "E" | "exec"                                   {E} (*Execute – same as Call (?)*)
    | "W" | "wait"                                   {W} (*Wait – behind a bareer*)
    | "@"                                            {T} (*Load – the content of some variable*)
    | "+"                                            {PLUS}
    | "-"                                            {MINUS}
    | "*"                                            {TIMES}
    | "/"                                            {DIVIDE}
    | "abs"                                          {ABS} (*absolute*)
    | "<=" | "⩽"                                     {LEQ} (*less or equal test*)
    | ">=" | "⩾"                                     {GEQ} (*less or equal test*)
    | "<"                                            {LS}
    | ">"                                            {GS}
    | "=="                                           {EQ} (*equal test*)
    | "<>" | "≠"                                     {NEQ} (*not equal test*)
    | "not" | "¬"                                    {NOT} (*boolean negation*)
    | "#void" | "ø" | "Ø"                            {VOID} (*empty process*)
    | ":"                                            {DDOT}
    | ";"                                            {SC}
    | "."                                            {DOT}
    | ","                                            {COMMA}
    | "="                                            {DEFINE}
    | ":="                                           {DDEFINE}
    | "|" | "⋁"                                      {OR} (*Boolean Or / Parallel composition*)
    | "&" | "⋀"                                      {AND} (*Boolean And*)
    | "?"                                            {QM} (*Question Mark*)
    | ("#incl"|"#include")[' ']([^'\n']* as f)       {INCLUDE f}
    | "#mutex" | "#mtx"                              {MUTEX}
    | "#mutex_recursive" | "#mtxr"                   {MUTEX_REC}
    | "#mutex_normal" | "#mtxn"                      {MUTEX_NORMAL}
    | "#mutex_errorcheck" | "#mtxec"                 {MUTEX_EC}
    | "#semaphore" | "#sem"                          {SEMAPHORE}
    | "#semaphore_quantitative" | "#semq"            {SEMAPHORE_QUANT}
    | "#monitor" | "#mntr"                           {MONITOR}
    | "#synchronization" | "#syn"                    {SYNCHRONIZATION}
    | "#fifo" | "#queue"                             {FIFO}
    | "#lifo" | "#stack"                             {LIFO}
    | ident as ident                                 {IDENT ident}
    | "{"([^'}']* as msg)"}"                         {MSG msg}
    | eof                                            {EOF}
    | _                                              {INVALID_CHARACTER}
