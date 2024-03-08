include module type of Do_intf

module Make (Client : Cohttp_lwt.S.Client) : sig
  type t

  val make : token:string -> t

  val account :
    t ->
    ( account,
      [> status_error Schematic_http.Error.reason ] Schematic_http.Error.t )
    Lwt_result.t

  val invoices :
    ?max:int ->
    t ->
    ( invoice,
      [> status_error Schematic_http.Error.reason ] Schematic_http.Error.t )
    Sequence.t

  val invoice_pdf :
    t ->
    string ->
    ( string Lwt_stream.t,
      [> status_error Schematic_http.Error.reason ] Schematic_http.Error.t )
    Lwt_result.t

  module Domains : sig
    val list :
      ?max:int ->
      t ->
      ( domain,
        [> status_error Schematic_http.Error.reason ] Schematic_http.Error.t )
      Sequence.t
  end
end

val pp_error : error Fmt.t
val pp_do_error : do_error Fmt.t
