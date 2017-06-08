(*This file should remain autonomous*)

module Gui =
struct
  let fullscreen = ref false
end (*Gui*)

module Shell =
struct
  let color = ref true (*Enable (true) / disable (false) colors*)
  let utf8  = ref true (*Enable (true) / disable (false) UTF-8*)
end (*Shell*)

module Directory =
struct
  let home             = ref "/home/emmanuel/"
  let configuration    = ref (!home^".dgrog/")
  let source_tests     = ref (!home^"/Bureau/wip/alcool/people/ehaucourt/geometric_realization/test/")
  let expected_results = ref (!home^"/Bureau/wip/alcool/people/ehaucourt/geometric_realization/test/expected_result/")
  let html             = ref (!home^"/Bureau/wip/alcool/people/ehaucourt/geometric_realization/html_ouput/tmp/")
end (*Directory*)

module Suffix =
struct
  let oda      = ref ".oda"
  let dgrog    = ref ".dgr"
  let cpodgrog = ref ".cpr"
  let pv       = ref ".pv"
end (*Suffix*)

module Display =
struct
  let verbose = ref false
  let pretty  = ref true
end (*Display*)
