module DDInt = 
  struct
    include DashDot(Integer)
    let of_string s: string -> t = failwith "NIY"
  end (* DDIint *)
