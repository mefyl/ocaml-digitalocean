(lang dune 3.0)

(name do)

(generate_opam_files true)

(license GPL-3.0-or-later)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(version 0.1.0)

(homepage https://github.com/mefyl/ocaml-do)

(bug_reports https://github.com/mefyl/ocaml-do/-/issues)

(source
 (uri git+https://github.com/mefyl/ocaml-do))

(package
 (name do)
 (synopsis "Digital Ocean API")
 (depends
  (acid
   (and
    (>= 0.11.0)
    (< 0.12.0)))
  cohttp-lwt
  fmt
  (schematic
   (and
    (>= 0.19.0)
    (< 0.20.0)))
  (schematic-http
   (and
    (>= 0.19.0)
    (< 0.20.0)))
  (timmy
   (and
    (>= 1.0.0)
    (< 2.0.0)))))

(package
 (name do-cli)
 (synopsis "Digital Ocean API CLI")
 (depends
  (cmdliner
   (and
    (>= 1.1.0)
    (< 1.2.0)))
  cohttp-lwt-unix
  do
  tls))
