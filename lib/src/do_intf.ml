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

and domain_record = {
  id : int;
  type' : domain_record_type; [@schematic.as "type"]
  name : string;
  data : string;
  priority : int option;
  port : int option;
  ttl : int;
  weight : int option;
  flags : int option;
  tag : string option;
}

and domain_record_creation = {
  type' : domain_record_type; [@schematic.as "type"]
  name : string;
  data : string;
  priority : int option;
  port : int option;
  ttl : int option;
  weight : int option;
  flags : int option;
  tag : string option;
}

and domain_record_type =
  | A [@schematic.as "A"]
  | Aaaa [@schematic.as "AAAA"]
  | Caa [@schematic.as "CAA"]
  | Cname [@schematic.as "CNAME"]
  | Mx [@schematic.as "MX"]
  | Ns [@schematic.as "NS"]
  | Soa [@schematic.as "SOA"]
  | Srv [@schematic.as "SRV"]
  | Txt [@schematic.as "TXT"]

and invoice = {
  uuid : string; [@schematic.as "invoice_uuid"]
  amount : float; [@schematic.schema fun _ -> Schemas.amount]
  invoice_period : int * Timmy.Month.t;
      [@schematic.schema fun _ -> Schemas.period]
}

and region = string
and status = Active | Warning | Locked [@@deriving schema]

and size = {
  slug : string;
  memory : int;
  vcpus : int;
  disk : int;
  transfer : int;
  price_monthly : int;
  price_hourly : float;
  regions : region list;
  available : bool;
  description : string;
}

module Sequence = Acid.Sequence.Make2 (Acid.Lwt_result)

type do_error = { id : string; message : string; request_id : string option }
[@@deriving schema]

type status_error =
  (do_error, Schematic_http.Error.never) Schematic_http.Error.status

type error = status_error Schematic_http.Error.reason Schematic_http.Error.t
