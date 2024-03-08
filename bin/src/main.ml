open Acid.Versions.V0_11_11
module Do_client = Do.Make (Cohttp_lwt_unix.Client)
module Arg = Cmdliner.Arg
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term

(* Arguments *)

let debug = Arg.(value @@ flag @@ info ~doc:"Show debug logs" [ "d"; "debug" ])
let domain = Arg.(info ~doc:"The name of the domain itself" [])

let domain_record_id =
  Arg.(info ~doc:"The unique identifier of the domain record." [])

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
  and domain_records =
    let opt_name =
      Arg.(
        opt (some string) None
        @@ info
             ~doc:
               "The host name, alias, or service being defined by the record."
             [ "name" ])
    and opt_type =
      Arg.(
        opt
          (enum
             Do.
               [
                 ("A", A);
                 ("AAAA", Aaaa);
                 ("CAA", Caa);
                 ("CNAME", Cname);
                 ("MX", Mx);
                 ("NS", Ns);
                 ("SOA", Soa);
                 ("SRV", Srv);
                 ("TXT", Txt);
               ])
          Do.A
        @@ info
             ~doc:
               "The host name, alias, or service being defined by the record."
             [ "type" ])
    and opt_data =
      Arg.(
        opt (some string) None
        @@ info
             ~doc:
               "Variable data depending on record type. For example, the \
                \"data\" value for an A record would be the IPv4 address to \
                which the domain will be mapped. For a CAA record, it would \
                contain the domain name of the CA being granted permission to \
                issue certificates."
             [ "data" ])
    and opt_domain =
      Arg.(info ~doc:"The name of the domain itself" [ "domain" ])
    in
    let create =
      let f domain name type' data =
        let f api =
          Do_client.Domain_records.create api ~domain
            {
              name;
              type';
              data;
              priority = None;
              port = None;
              ttl = None;
              weight = None;
              flags = None;
              tag = None;
            }
        in
        run Do.domain_record_schema f
      in
      term
        Term.(
          const f
          $ Arg.(required @@ opt (some string) None opt_domain)
          $ Arg.required opt_name $ Arg.value opt_type $ Arg.required opt_data)
    and delete =
      let f domain id =
        let f api = Do_client.Domain_records.delete api ~domain id in
        run Schematic.Schemas.unit_schema f
      in
      term
        Term.(
          const f
          $ Arg.(required @@ pos 0 (some string) None domain)
          $ Arg.(required @@ pos 1 (some int) None domain_record_id))
    and list =
      let f domain =
        let f api =
          Do_client.Domain_records.list api ~domain |> Do.Sequence.to_list
        in
        run (Schematic.Schemas.list_schema Do.domain_record_schema) f
      in
      term Term.(const f $ Arg.(required @@ pos 0 (some string) None domain))
    in
    Cmdliner.Cmd.group
      Cmd.(
        info
          ~doc:
            "Domain record resources are used to set or retrieve information \
             about the individual DNS records configured for a domain."
          "domain-records")
      ~default:list
      [
        command ~doc:"Create a new record in a domain." "create" create;
        command ~doc:"Delete a record for a domain." "delete" delete;
        command ~doc:"List all records configured for a domain." "list" list;
      ]
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
  Cmdliner.Cmd.group info
    [ account; domains; domain_records; invoices; invoice_pdf ]

let () = Stdlib.exit @@ Cmd.eval_result commands
