#
# Makefile

all:
	ocamlbuild -use-ocamlfind -pkgs ANSITerminal,str,lwt,lwt.syntax,lwt.unix,xml-light,cohttp,cohttp.lwt,lablgtk2 -syntax camlp4o run_server.byte
	ocamlbuild -use-ocamlfind -pkgs notty,notty.lwt,ANSITerminal,str,lwt,lwt.syntax,lwt.unix,lwt.glib,lablgtk2 -syntax camlp4o gui_entry.byte
	mv run_server.byte camlserver
	mv gui_entry.byte camlclient

server:
	ocamlbuild -use-ocamlfind -pkgs ANSITerminal,str,lwt,lwt.syntax,lwt.unix,xml-light,cohttp,cohttp.lwt -syntax camlp4o run_server.byte
	mv run_server.byte camlserver

client:
	ocamlbuild -use-ocamlfind -pkgs notty,notty.lwt,ANSITerminal,str,lwt,lwt.syntax,lwt.unix,lwt.glib,lablgtk2 -syntax camlp4o gui_entry.byte
	mv gui_entry.byte camlclient

indent: *.ml
	for mlfile in $^; do ocp-indent -i --syntax lwt $$mlfile; done

clean:
	ocamlbuild -clean

# vim:ft=make
#
