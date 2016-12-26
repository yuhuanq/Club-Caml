#
# Makefile

BUILD_DIR = _build

SRC_DIR = src

FLAGS = -use-ocamlfind

OCAMLBUILD ?= ocamlbuild

all:
	$(OCAMLBUILD) $(FLAGS) -pkgs str,lwt,lwt.syntax,lwt.unix,xml-light,cohttp,cohttp.lwt,lablgtk2 -syntax camlp4o $(SRC_DIR)/run_server.byte
	$(OCAMLBUILD) $(FLAGS) -pkgs str,lwt,lwt.syntax,lwt.unix,lwt.glib,lablgtk2 -syntax camlp4o $(SRC_DIR)/gui_entry.byte
	mv run_server.byte camlserver
	mv gui_entry.byte camlclient

server:
	$(OCAMLBUILD) $(FLAGS) -pkgs str,lwt,lwt.syntax,lwt.unix,xml-light,cohttp,cohttp.lwt,lablgtk2 -syntax camlp4o $(SRC_DIR)/run_server.byte
	mv run_server.byte camlserver

client:
	$(OCAMLBUILD) $(FLAGS) -pkgs str,lwt,lwt.syntax,lwt.unix,lwt.glib,lablgtk2 -syntax camlp4o $(SRC_DIR)/gui_entry.byte
	mv gui_entry.byte camlclient

clean:
	$(OCAMLBUILD) -clean

# vim:ft=make
#
