#
# Makefile
# yqiu, 2016-10-27 18:33
#

all:
	# ocamlbuild -use-ocamlfind -pkgs str,lwt,lwt.syntax,lwt.unix server.byte client.byte protocol.byte
	ocamlbuild -use-ocamlfind -pkgs str,lwt,lwt.syntax,lwt.unix -syntax camlp4o server.byte client.byte protocol.byte
	# ocamlbuild -use-ocamlfind -classic-display -tag 'syntax(camlp4o)' -package str,lwt.syntax,lwt.unix,lwt client.byte protocol.byte

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
