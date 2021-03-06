open Core.Std

type action = | Done
              | Fail
              | Help
              | Start

let parse_action = function
  | [_; "d"] | [_; "done"]        -> Done
  | [_; "f"] | [_; "fail"]        -> Fail
  | [_] | [_; "s"] | [_; "start"] -> Start
  | _                             -> Help

let help =
  let prog = Sys.executable_name in
  Ok [ "Usage:"
     ; (sprintf "%s [start] - start a new flow" prog)
     ; (sprintf "%s d[one] - record a successful flow" prog)
     ; (sprintf "%s f[ail] - fail the current flow" prog)
     ]

(* This is ridiculous, but I can't find a way to forrmat
   the time using built in functions, neither a way to deconstruct
   it into managable pieces. *)
let format_time datetime =
  let raw = Time.to_string datetime in
  let date = String.sub raw ~pos:0 ~len:10 in
  let time = String.sub raw ~pos:11 ~len:8 in
  let zone_offset = String.length raw - 6 in
  let zone = String.sub raw ~pos:zone_offset ~len:6 in
  sprintf "%sT%s%s" date time zone

let now () = Core.Time.now ()

let rec find_home arr i =
  if i < 0 then failwith "HOME not set"
  else
    match String.split ~on:'=' arr.(i) with
    |  ["HOME"; home] -> home
    | _               -> find_home arr (i-1)

let home () =
  let env = Unix.environment () in
  let index = (Array.length env) - 1 in
  find_home env index

let flow_path () =
  (sprintf "%s/.flow" (home ()))
let ledger_path () =
  (sprintf "%s/flow_ledger.csv" (home ()))

let read_flow_file () =
  In_channel.with_file (flow_path ()) ~f:(fun inc -> input_line inc)

let write_ledger start_time end_time duration =
  Out_channel.with_file ~binary:false ~append:true ~perm:0o600 ~f:(fun out ->
    Out_channel.output_string out (sprintf "%s,%s,%d\n" start_time end_time duration)
  ) (ledger_path ())

let remove_flow_file () =
  Unix.unlink (flow_path ())

let finish () =
  let end_time = now () in
  let start_time_str = read_flow_file () in
  let start_time = Time.of_string start_time_str in
  let diff = Int.of_float (Time.Span.to_sec (Time.diff end_time start_time)) in
  if diff < 0 then
    Error [  "Time went backwards?" ]
  else
    let () = write_ledger start_time_str (format_time end_time) diff in
    let () = remove_flow_file () in
    Ok [ (sprintf "Flow finished in %d seconds" diff) ]

let open_flow_file () =
  let flags = [ Open_wronly
              ; Open_creat
              ; Open_excl
              ; Open_text
              ] in
  open_out_gen flags 0o666 (flow_path ())

let write_flow_file () =
  let to_write = format_time (now ()) in
  let out = open_flow_file () in
  protect ~f:(fun () -> output_string out to_write)
          ~finally:(fun () -> Out_channel.close out)

let start_error_msg msg =
  match String.substr_index msg ~pattern:"File exists" with
    | Some _ -> [(sprintf "Flow already started at %s." (read_flow_file ()))]
    | None   -> ["Failed to create flow file:"; msg]

let start () =
  try
    write_flow_file ();
    Ok []
  with Sys_error msg ->
    Error (start_error_msg msg)
  
let fail () =
  try
    remove_flow_file ();
    Ok []
  with
    Unix.Unix_error _ ->
      Error ["Failed to remove flow file"]

let do_action = function
  | Done  -> finish ()
  | Fail  -> fail ()
  | Help  -> help
  | Start -> start ()

let print_lines lines =
  List.iter ~f:(fun l -> printf "%s\n" l) lines

let handle_action_result = function
  | Ok output    -> print_lines output
  | Error output ->
      print_lines output;
      exit 1

let () =
  parse_action (Array.to_list Sys.argv)
  |> do_action
  |> handle_action_result
