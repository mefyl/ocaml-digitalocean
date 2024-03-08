open Acid.Versions.V0_11_11
include Do_intf

let uri ?query fmt =
  let f path =
    Uri.make ~scheme:"https" ~host:"api.digitalocean.com" ~path:("/v2/" ^ path)
      ?query ()
  in
  Fmt.kstr f fmt

module Check_status_code = struct
  type error = do_error

  let check status =
    if Cohttp.Code.(is_success (code_of_status status)) then `Ok
    else `Body_error do_error_schema
end

module Make (Client : Cohttp_lwt.S.Client) = struct
  open Let.Syntax2 (Lwt_result)

  module Http_handler =
    Schematic_http.Check_status
      (Check_status_code)
      (Schematic_http.Cohttp (Client) (Schematic.Json.String))

  module type HTTP =
    Schematic_http.S with type 'a t = 'a and type error = status_error

  type t = { http : (module HTTP); token : string }

  type links = { pages : pages option }

  and pages = {
    first : Uri.t option;
    last : Uri.t option;
    next : Uri.t option;
    prev : Uri.t option;
  }
  [@@deriving schema]

  (* DO has a default page size of 20, honor it by default *)
  let page_size = 20

  let paginate ?max { http = (module Http); _ } schema split uri =
    let f = function
      | None, _ | _, Some 0 -> Lwt_result.return Sequence.Step.Done
      | Some uri, max ->
          let max, query =
            match max with
            | None -> (None, None)
            | Some max when max >= page_size -> (Some (max - page_size), None)
            | Some max ->
                ( Some 0,
                  Some
                    (Some (("per_page", [ Int.to_string max ]) :: Uri.query uri))
                )
          in
          let+ page = Http.get schema (Uri.with_uri ?query uri) in
          let items, { pages; _ } = split page in
          Sequence.Step.Yields
            (items, (Option.bind ~f:(fun { next; _ } -> next) pages, max))
    in
    Sequence.unfold ~init:(Some uri, max) ~f

  let make ~token =
    let module Http =
      Schematic_http.Make
        (Schematic_http.With_headers
           (struct
             let headers = [ ("Authorization", Fmt.str "Bearer %s" token) ]
           end)
           (Http_handler)) in
    { http = (module Http); token }

  type account_response = { account : account } [@@deriving schema]

  let account { http = (module Http); _ } =
    let+ { account } = Http.get account_response_schema @@ uri "account" in
    account

  type invoices_response = { invoices : invoice list; links : links }
  [@@deriving schema]

  let invoices ?max t =
    paginate ?max t invoices_response_schema (fun { invoices; links } ->
        (invoices, links))
    @@ uri "customers/my/invoices"

  let invoice_pdf { http = (module Http); _ } uuid =
    Http.get Http.stream @@ uri "customers/my/invoices/%s/pdf" uuid

  module Domains = struct
    type domains_response = { domains : domain list; links : links }
    [@@deriving schema]

    let list ?max t =
      paginate ?max t domains_response_schema (fun { domains; links } ->
          (domains, links))
      @@ uri "domains"
  end

  module Domain_records = struct
    type domain_record_response = { domain_record : domain_record }

    and domain_records_response = {
      domain_records : domain_record list;
      links : links;
    }
    [@@deriving schema]

    let create { http = (module Http); _ } ~domain record =
      let+ { domain_record } =
        Http.post
          ~body:(domain_record_creation_schema, record)
          domain_record_response_schema
        @@ uri "domains/%s/records" domain
      in
      domain_record

    let delete { http = (module Http); _ } ~domain id =
      Http.delete Http.empty @@ uri "domains/%s/records/%d" domain id

    let list ?max t ~domain =
      paginate ?max t domain_records_response_schema
        (fun { domain_records; links } -> (domain_records, links))
      @@ uri "domains/%s/records" domain
  end
end

let pp_do_error = Fmt.using (fun { message; _ } -> message) Fmt.string
let pp_error = Schematic_http.Error.(pp (pp_reason Fmt.nop))
