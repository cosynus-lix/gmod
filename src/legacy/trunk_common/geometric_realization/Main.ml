let usage_msg = ""
(*
"Testing the library dealing with isothetic regions.
You should provide a file. Allowed extensions are .oda, .dgr, and .cpr."
*)

let anon_fun filename =
	Command.Flag.filename := Some filename ;
	Command.SetFlag.parsing_mode ()

let last_arg () =
  let n = Array.length Sys.argv in
  Array.get Sys.argv (n - 1)

let before_last_arg () =
  let n = Array.length Sys.argv in
  Array.get Sys.argv (n - 2)

(*Entry point of the program*)

let () =
  if Array.length Sys.argv <> 1
  then
     try
      let () = Arg.parse CommandLineOptions.list_of_options anon_fun usage_msg in
      Command.go ()
     with
      | Sys_error _ ->
          let filename = Command.get_some !Command.Flag.filename in
          Printf.printf "%s Unable to access file %s located in directory %s\n"
          Message.error (Message.orange (Filename.basename filename)) (Message.orange (Filename.dirname filename))
      | Failure "Unknown file format" ->
        let filename = Command.get_some !Command.Flag.filename in
        let e = Message.error in
        let f = Message.yellow filename in
        Printf.printf "%s the suffix of the filename %s is not valid\n" e f
      | Failure "Missing filename" ->
        let e = Message.error in
        Printf.printf "%s Some option requires you provide a file as last argument of the command line:
  – the argument %s has been consumed as an argument by the option %s\n"
  e (Message.orange (last_arg ())) (Message.orange (before_last_arg ()))
  else
    Printf.printf "Any file whose suffix is either %s, %s, %s, or %s is treated accordingly.
Try the option %s/%s for more details.\n"
    (Message.hl ".oda")
    (Message.hl ".dgr")
    (Message.hl ".cpr")
    (Message.hl ".pv")
    (Message.hl "-help")
    (Message.hl "--help")


(*
Principe de fonctionnement:

 1 – La fonction Arg.parse lit les options passées dans la ligne de commande et 
 appelle alors la fonction sf correspondante dans le module SetFlag. Lorsque 
 l'option est assortie d'un argument a, la fonction sf renseigne une référence 
 se trouvant dans le module Flag. Si l'option doit déclencher un calcul et ou 
 un affichage, des fonctions contenues dans le module Proceed sont empilées 
 dans la liste actions_to_perform. Les fonctions du module Proceed sont toutes 
 de type unit -> unit. Les arguments dont les fonctions du module Proceed 
 pourraient avoir besoin sont "calculés" par d'autres fonctions du module 
 Proceed. Plus précisément, ces fonctions renseignent des références du module 
 Data. Certaines fonctions du module SetFlag ont besoin que 
 Data.abstract_syntax ait été renseigné, dans ce cas il faut empiler les effets 
 de bords dans action_to_perform.

 2 – Les calculs se font par "effet de bords", c'est-à-dire que des références 
 –dans le module Data– sont prévues pour recevoir les résultats des calculs. 
 Cela permet la réutilisation des résultats déjà calculés.

 3 – L'affichage se fait dans l'ordre selon lequel les options ont été passées, 
 pour cela, les effets de bords sont stockés dans une liste au fur et à mesure 
 que la fonction Arg.parse les lit.

 L'intérêt d'utiliser des références réside dans la possibilité de réutiliser 
 certains résultats intermédiaires nécessaire à plusieurs calculs.
*)
