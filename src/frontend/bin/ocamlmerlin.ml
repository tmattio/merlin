let () = 
  let argc = (Array.length Sys.argv) in
  let argv = Sys.argv in
  if argc >= 2 && Array.get argv 1 = "server" then
    Ocamlmerlin.run_client (argc - 2) (Array.sub argv 2 (argc -2))
  else
    Ocamlmerlin.run_server ()
