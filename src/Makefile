#
# Makefile
# yqiu, 2016-10-27 18:33
# ewang, 2016-11-24 6:43
# sb892, 2016-11-26 19:31

all:
	ocamlbuild -use-ocamlfind -pkgs str,lwt,lwt.syntax,lwt.unix -syntax camlp4o run_server.native
	ocamlbuild -use-ocamlfind -pkgs ANSITerminal,str,lwt,lwt.syntax,lwt.unix -syntax camlp4o client.native

server:
	ocamlbuild -use-ocamlfind -pkgs str,lwt,lwt.syntax,lwt.unix -syntax camlp4o run_server.native

client:
	ocamlbuild -use-ocamlfind -pkgs ANSITerminal,str,lwt,lwt.syntax,lwt.unix -syntax camlp4o client.native

test:
	# ocamlbuild use-ocamlfind -pkgs str,lwt,lwt.unix,oUnit test_protocol.byte && ./test_protocol.byte
	# doesn't work atm without runner sequential..............
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix,oUnit,str test_protocol.byte && ./test_protocol.byte -runner sequential

indent: *.ml
	for mlfile in $^; do ocp-indent -i --syntax lwt $$mlfile; done

clean:
	ocamlbuild -clean

# vim:ft=make
#
