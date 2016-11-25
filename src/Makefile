#
# Makefile
# yqiu, 2016-10-27 18:33
#

all:
	ocamlbuild -use-ocamlfind -pkgs str,lwt,lwt.unix server.byte client.byte protocol.byte newclient.byte

test:
	# ocamlbuild use-ocamlfind -pkgs str,lwt,lwt.unix,oUnit test_protocol.byte && ./test_protocol.byte
	# doesn't work atm without runner sequential..............
	ocamlbuild -pkgs lwt,lwt.unix,oUnit,str test_protocol.byte && ./test_protocol.byte -runner sequential

clean:
	ocamlbuild -clean


# vim:ft=make
#
