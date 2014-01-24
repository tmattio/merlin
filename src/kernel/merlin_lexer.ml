open Std

type keywords = Raw_lexer.keywords

(* Lexing step *)
type item =
  | Valid of Lexing.position * Raw_parser.token * Lexing.position
  | Error of Raw_lexer.error * Location.t

let item_start (Valid (p,_,_) | Error (_,{Location. loc_start = p})) =
  p

let item_end (Valid (_,_,p) | Error (_,{Location. loc_end = p})) =
  p

(** Create an empty list new lexer *)
let empty ~filename =
  let pos =
    { Lexing.
      pos_fname = filename;
      pos_lnum  = 1;
      pos_bol   = 0;
      pos_cnum  = 0;
    }
  in
  History.initial (Valid (pos, Raw_parser.ENTRYPOINT, pos))

type t = {
  (* Result *)
  mutable history: item History.t;
  (* Input buffer *)
  refill: string option ref; (* Input not yet sent to lexer *)
  refill_empty: bool ref;    (* Lexer internal buffer status *)
  (* Lexer data *)
  state: Raw_lexer.state;
  lexbuf: Lexing.lexbuf;
  mutable resume: (unit -> Raw_parser.token Raw_lexer.result) option;
}

let history t = t.history

(** Prepare for lexing.
    Returns the start position (end position of last valid token), and a
    lexing function that will append at most one token to the history at each
    call. *)
let make_lexbuf empty refill position =
  Lexing.from_strings ~position ~empty ""
    (fun () ->
       match !refill with
       | Some s -> refill := None; s
       | None -> "")

let start keywords history =
  let position = match History.focused history with
    | Valid (_,_,p) -> p
    | Error (_,l) -> l.Location.loc_end
  in
  let refill = ref None in
  let refill_empty = ref true in
  let lexbuf = make_lexbuf refill_empty refill position in
  {
    history;
    state = Raw_lexer.make keywords;
    resume = None; refill; refill_empty; lexbuf;
  }

let position t = Lexing.immediate_pos t.lexbuf

let feed t str =
  if not t.lexbuf.Lexing.lex_eof_reached then begin
    t.refill := Some str;
    let append item =
      t.history <- History.insert item t.history
    in
    let rec aux = function
      (* Lexer interrupted, there is data to refill: continue. *)
      | Raw_lexer.Refill f
        when !(t.refill) <> None || not !(t.refill_empty) ->
        aux (f ())
      (* Lexer interrupted, nothing to refill, return to caller. *)
      | Raw_lexer.Refill r ->
        t.resume <- Some r
      (* EOF Reached: notify EOF to parser, stop now *)
      | Raw_lexer.Return Raw_parser.EOF ->
        begin match History.focused t.history with
          | Valid (_,Raw_parser.EOF,_) -> ()
          | _ ->
            append (Valid (t.lexbuf.Lexing.lex_start_p,
                           Raw_parser.EOF,
                           t.lexbuf.Lexing.lex_curr_p));
        end
      | Raw_lexer.Return token ->
        append (Valid (t.lexbuf.Lexing.lex_start_p,
                       token,
                       t.lexbuf.Lexing.lex_curr_p));
        continue ()
      | Raw_lexer.Error (e,l) ->
        append (Error (e,l));
        continue ()
    and continue () =
      aux (Raw_lexer.token t.state t.lexbuf)
    in
    begin match t.resume with
    | Some f ->
      t.resume <- None;
      aux (f ())
    | None -> continue ()
    end;
    true
  end
  else
    false

let eof t = t.lexbuf.Lexing.lex_eof_reached
