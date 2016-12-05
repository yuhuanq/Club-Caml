# Club Caml

Chat, Play, Socialize.

STOMP and IRC inspired Instant Messaging Client and Server Application.

The goal was to implement an existing Messaging Protocol (STOMP) and extend it.
The client goal was a minimalistic graphical user interface. Server goal is
robustness and scalability.

## Compilation
`make all` to compile both the client and server

`make client` to compile the client

`make server` to compile the server

### Required opam packages

```
lwt v2.5.2
lablgtk v2.18.5 (note that the gtk+2.0 package must be installed)
camlp4 v4.03+1
cohttp v0.21.0
xml-light v2.4
notty v0.1.1
```

## Overview

To start the server, run i.e. `./camlserver --verbose`.

`./camlclient` for the client.

The client will prompt one for a username and address/port combination.
Once in the lobby, one can select different chatrooms to join, including a
chatbot.

Invoke `#help` to see a list of directives.

#####Please note:
If you are running the server and client both locally, do not change the
address and port combination that is default (127.0.0.1 is localhost), and
default server port is 9000.

##Fun Things to try
####Chatbot room
Join the chatbot room to talk with
a chat bot! This chatbot uses the
*Pandorabots API* to select its responses.
####Easter eggs
Try sending "🐪" or "😶" for something interesting! ;)
