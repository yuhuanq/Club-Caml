# Club Caml

Chat, Play, Socialize.

STOMP and IRC inspired Instant Messaging Client and Server Application.

The goal was to implement an existing Messaging Protocol (STOMP) and extend it.
The client goal was a minimalistic graphical user interface. Server goal is
robustness and scalability.

## Compilation
`make all` to compile both the client and server

`make client`

`make server`

`make install`

### Required packages

```
lwt
lablgtk
camlp4o
cohttp
xml-light
```

## Overview

To start the server, run i.e. `bin/camlserver --verbose`.

`bin/camlclient` for the client.

The client will prompt you for a username and you will then arrive in the lobby
where you can select different chatrooms to join, including a chatbot.

Invoke `#help` to see a list of directives.

