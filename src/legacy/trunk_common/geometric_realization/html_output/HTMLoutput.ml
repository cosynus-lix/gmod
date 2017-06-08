(* Create an html page from any calculation sheet. *)

module E = Expression

(* fs is the font size factor, given as a percentage. *)

let head fs = Printf.sprintf
  "
  <script src=\"jquery-latest.js\"></script>
  <link rel=\"stylesheet\" href=\"jquery.treeview.css\" type=\"text/css\" media=\"screen\" />
  <script type=\"text/javascript\" src=\"jquery.treeview.js\"></script>
  <style type=\"text/css\">
  #browser {
    font-family: Verdana, helvetica, arial, sans-serif;
    font-size: %f%%;
  }
  </style>
  <script>
  $(document).ready(function(){
    $(\"#browser\").treeview();
 $(\"#add\").click(function() {
 	var branches = $(\"<li><span class='folder'>New Sublist</span><ul>\" + 
 		\"<li><span class='file'>Item1</span></li>\" + 
 		\"<li><span class='file'>Item2</span></li>\" +
 		\"</ul></li>\").appendTo(\"#browser\");
 	$(\"#browser\").treeview({
 		add: branches
 	});
 });
  });
  </script>
"
    (if fs < 0. then 100. else fs)

let example = Printf.sprintf
  "
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" 
                    \"loose.dtd\">
<html>
<head>%s</head>
<body>
  
  <ul id=\"browser\" class=\"filetree\">

 	<li><span class=\"folder\">Folder 1</span>
 		<ul><li><span class=\"file\">Item 1.1</span></li></ul>
 	</li>

 	<li>
            <span class=\"folder\">Folder 2</span>
 		<ul>
 			<li><span class=\"folder\">Subfolder 2.1</span>
 				<ul id=\"folder21\">
 					<li><span class=\"file\">File 2.1.1</span></li>
 					<li><span class=\"file\">File 2.1.2</span></li>
 				</ul>
 			</li>
 			<li><span class=\"file\">File 2.2</span></li>
 		</ul>
 	</li>

 	<li class=\"closed\"><span class=\"folder\">Folder 3 (closed at start)</span>
 		<ul>
 			<li><span class=\"file\">File 3.1</span></li>
 		</ul>
 	</li>
 	<li><span class=\"file\">File 4</span></li>
   </ul>

</body>
</html>"
  (head 100.)

let an_entry v ar = Printf.sprintf 
  "
<li class=\"closed\">
  <span class=\"folder\">%s</span>
  <ul><li><span class=\"file\">%s</span></li></ul>
</li>
"
  v
  (BuiltIn.ODA.OverInteger.HL.string_of ar)

let the_entries context = E.fold (fun v ar accu -> accu^(an_entry v ar)^"\n") context ""

let body context = Printf.sprintf
  "<ul id=\"browser\" class=\"filetree\">%s</ul>"
  (the_entries context)

let html_of context = Printf.sprintf
  "
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" 
                    \"loose.dtd\">
<html>
<head>%s</head>
<body><ul id=\"browser\" class=\"filetree\">%s</ul></body>
</html>
"
  (head 100.)
  (body context)

let htmlfile_of ?(output_directory= !Parameter.Directory.html) ?(output_file="tmp.html") context =
  let c_out = open_out_bin (output_directory^output_file) in
  output_string c_out (html_of context);
  close_out c_out
