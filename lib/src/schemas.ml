open Base

let try_with ~error f v = try Result.Ok (f v) with _ -> Result.fail error
let ( let* ) = Result.( >>= )

let amount =
  Schematic.Schema.make
    (Map
       {
         decode =
           (fun s ->
             try_with ~error:(Fmt.str "invalid amount: %s" s) Float.of_string s);
         encode = Float.to_string;
         descriptor = String;
       })

let period =
  let decode s =
    match Base.String.split ~on:'-' s with
    | [ year; month ] ->
        let* year =
          try_with ~error:"invalid period year" Base.Int.of_string year
        in
        let* month =
          try_with ~error:"invalid period month" Base.Int.of_string month
        in
        Base.Result.map (Timmy.Month.of_int month) ~f:(fun month ->
            (year, month))
    | _ -> Result.failf "invalid period: %s" s
  in
  Schematic.Schema.make
    (Map
       {
         decode;
         encode =
           (fun (year, month) ->
             Fmt.str "%4d-%02d" year Timmy.Month.(to_int month));
         descriptor = String;
       })
