#
# Makefile
# yqiu, 2016-10-27 18:33
#

all:
	ocamlbuild -use-ocamlfind -pkgs lwt,lwt.unix server.byte client.byte

test:
	ocamlbuild -use-ocamlfind -pkgs oUnit test_protocol.byte && ./test_protocol.byte


# vim:ft=make
#
