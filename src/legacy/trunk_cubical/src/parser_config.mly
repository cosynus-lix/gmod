%{
  open Globals
%}

%token <unit> UNIT
%token <bool> BOOL
%token <int>  INT
%token <string> PATH
%token GUI_WINDOW_SIZE SCREEN_SHELL_WIDTH HOME TERMINAL_COLOR
%token CALCUL TIKZ OOGL PV MIEL
%token END

%type < unit > config
%start config
%%

a_config_line :
| UNIT                   { () }
| PV     PATH            { Settings.pv_dir := $2 }
| TIKZ   PATH            { Settings.tikz_dir := $2 }
| OOGL   PATH            { Settings.oogl_dir := $2 }
| CALCUL PATH            { Settings.calculator_dir := $2 }
| MIEL   PATH            { Settings.miel_dir := $2 }
| TERMINAL_COLOR BOOL    { Settings.use_terminal_colors := $2 }
| SCREEN_SHELL_WIDTH INT { Settings.terminal_width := $2 }
  ;

  config:
| END {()}
| a_config_line END    {()}
| a_config_line config {()}
  ;

%%
