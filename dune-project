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
  (cohttp-lwt
   (and
    (>= 5.0.0)
    (< 5.1.0)))
  fmt
  (schematic
   (and
    (>= 0.13.0)
    (< 0.14.0)))
  (schematic-http
   (and
    (>= 0.13.0)
    (< 0.14.0)))
  (timmy
   (and
    (>= 0.12.0)
    (< 0.13.0)))))

(package
 (name do-cli)
 (synopsis "Digital Ocean API CLI")
 (depends
  (acid
   (and
    (>= 0.7.0)
    (< 0.8.0)))
  (cmdliner
   (and
    (>= 1.1.0)
    (< 1.2.0)))
  (cohttp-lwt-unix
   (and
    (>= 5.0.0)
    (< 5.1.0)))
  do
  tls))
