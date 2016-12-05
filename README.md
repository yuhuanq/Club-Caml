# Club Caml

![]( https://raw.githubusercontent.com/yuhuanq/Club-Caml/master/ex.gif?token=AQYGq4f9ZEAuP60oT3vYrqc4PM1on-6oks5YTxQhwA==)

Chat, Play, Socialize.

STOMP and IRC inspired Instant Messaging Client and Server Application.

## Compilation
`make all` to compile both the client and server

`make client` to compile the client

`make server` to compile the server

### Required OCaml packages

```
lwt.2.5.2
lablgtk.2.18.5
camlp4.4.03+1
cohttp.0.21.0
xml-light.2.4
notty.0.1.1
```

## Overview

To start the server, run i.e. `./camlserver --verbose`.

`./camlclient` for the client.

The client will prompt one for a username and address/port combination.
Once in the lobby, one can select different chatrooms to join, including a
chatbot.

Invoke `#help` to see a list of directives.

Try sending ":no_mouth:" for something interesting!

