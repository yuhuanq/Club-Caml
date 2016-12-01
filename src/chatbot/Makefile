#
# Makefile

all:
	ocamlbuild -use-ocamlfind -pkgs lwt,lwt.syntax,lwt.unix,cohttp -syntax camlp4o chatbot.byte

indent: *.ml
	for mlfile in $^; do ocp-indent -i --syntax lwt $$mlfile; done

clean:
	ocamlbuild -clean

# vim:ft=make
#
