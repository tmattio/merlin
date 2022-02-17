open Ocamlmerlin_server

let main () =
  (* Setup env for extensions *)
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  match List.tl (Array.to_list Sys.argv) with
  | "single" :: args -> exit (New.run ("PATH=" ^ Unix.getenv "PATH" ^ "\000") None args)
  | "old-protocol" :: args -> Old.run args
  | ["server"; socket_path; socket_fd] -> Server.start socket_path socket_fd
  | ("-help" | "--help" | "-h" | "server") :: _ ->
    Printf.eprintf
      "Usage: %s <frontend> <arguments...>\n\
       Select the merlin frontend to execute. Valid values are:\n\
      \n- 'old-protocol' executes the merlin frontend from previous version.\n\
      \  It is a top level reading and writing commands in a JSON form.\n\
      \n- 'single' is a simpler frontend that reads input from stdin,\n\
      \  processes a single query and outputs result on stdout.\n\
      \n- 'server' works like 'single', but uses a background process to\n\
      \  speedup processing.\n\
       If no frontend is specified, it defaults to 'old-protocol' for\n\
       compatibility with existing editors.\n"
      Sys.argv.(0)
  | args -> Old.run args

let () =
  let log_file =
    match Sys.getenv "MERLIN_LOG" with
    | exception Not_found -> None
    | file -> Some file
  in
  Merlin_merlin_utils.Logger.with_log_file log_file main
