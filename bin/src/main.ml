open Acid.Versions.V0_11_11
module Do_client = Do.Make (Cohttp_lwt_unix.Client)
module Arg = Cmdliner.Arg
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term

(* Arguments *)

let debug = Arg.(value @@ flag @@ info ~doc:"Show debug logs" [ "d"; "debug" ])

let invoice_uuid =
  Arg.(
    required
    @@ opt (some string) None
    @@ info ~doc:"Invoice UUID" [ "i"; "invoice" ])

let max =
  Arg.(
    value
    @@ opt (some int) None
    @@ info ~doc:"Maximum number of objects to retreive" [ "m"; "max" ])

let token =
  Arg.(
    required
    @@ opt (some string) None
    @@ info ~docs:"security" ~doc:"Do API token" [ "t"; "token" ])

let error_to_string =
  Fmt.to_to_string
    Schematic_http.Error.(pp (pp_reason (pp_status Do.pp_do_error Fmt.nop)))

(* let stream_channel channel =
 *   let open Let.Syntax (Lwt) in
 *   let seq =
 *     let f () =
 *       let+ read = Lwt_io.read ~count:4096 channel in
 *       if String.is_empty read then None else Some (read, ())
 *     in
 *     Lwt_seq.unfold_lwt f ()
 *   in
 *   Lwt_stream.of_lwt_seq seq *)

let commands =
  let open Let.Syntax2 (Lwt_result) in
  let term f = Term.(f $ debug $ token) in
  let command ~doc name term = Cmd.v (Cmd.info ~doc name) term
  and run_raw f debug token =
    let () = Logs.set_reporter @@ Logs_fmt.reporter () in
    let () = if debug then Logs.set_level ~all:true (Some Debug) in
    let api = Do_client.make ~token in
    let lwt = f api in
    Lwt_result.map_error error_to_string lwt |> Lwt_main.run
  in
  let run schema f debug token =
    let f api =
      let+ res = f api in
      Fmt.(
        pf stdout "@[%a@]@." Schematic.Json.pp
          (Schematic.Json.encode schema res))
    in
    run_raw f debug token
  and run_raw resp f debug token =
    let f api =
      let* res = f api in
      resp res
    in
    run_raw f debug token
  in
  let account =
    let f =
      let f api = Do_client.account api in
      run Do.account_schema f
    in
    command ~doc:"Provides information about your current account." "account"
      (term Term.(const f))
  and domains =
    let list =
      let f =
        let f api = Do_client.Domains.list api |> Do.Sequence.to_list in
        run (Schematic.Schemas.list_schema Do.domain_schema) f
      in
      term Term.(const f)
    in
    Cmdliner.Cmd.group
      Cmd.(
        info
          ~doc:
            "Domain resources are domain names that you have purchased from a \
             domain name registrar that you are managing through the \
             DigitalOcean DNS interface."
          "domains")
      ~default:list
      [ command ~doc:"List all domains in your account" "list" list ]
  and invoices =
    let f max =
      let f api = Do_client.invoices ?max api |> Do.Sequence.to_list in
      run (Schematic.Schemas.list_schema Do.invoice_schema) f
    in
    command ~doc:"Retrieve a list of all invoices." "invoices"
      (term Term.(const f $ max))
  and invoice_pdf =
    let f id =
      let f api = Do_client.invoice_pdf api id
      and resp stream =
        Lwt_stream.iter (Stdlib.output_string Stdlib.stdout) stream
        |> Lwt.map ~f:Result.return
      in
      run_raw resp f
    in
    command ~doc:"Retrieve a PDF for an invoice." "invoice-pdf"
      (term Term.(const f $ invoice_uuid))
  and info = Cmd.(info ~doc:"Do business API." "do") in
  Cmdliner.Cmd.group info [ account; domains; invoices; invoice_pdf ]

let () = Stdlib.exit @@ Cmd.eval_result commands
