type account = {
  droplet_limit : int;
  floating_ip_limit : int;
  email : string;
  uuid : string;
  email_verified : bool;
  status : status;
  status_message : string;
}

and domain = { name : string; ttl : Timmy.Span.t; zone_file : string }

and invoice = {
  uuid : string; [@schematic.as "invoice_uuid"]
  amount : float; [@schematic.schema fun _ -> Schemas.amount]
  invoice_period : int * Timmy.Month.t;
      [@schematic.schema fun _ -> Schemas.period]
}

and status = Active | Warning | Locked [@@deriving schema]

module Sequence = Acid.Sequence.Make2 (Acid.Lwt_result)

type do_error = { id : string; message : string; request_id : string option }
[@@deriving schema]

type status_error =
  (do_error, Schematic_http.Error.never) Schematic_http.Error.status

type error = status_error Schematic_http.Error.reason Schematic_http.Error.t
