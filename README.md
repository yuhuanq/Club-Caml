# Club Caml

![]( https://raw.githubusercontent.com/yuhuanq/Club-Caml/master/ex.gif?token=AQYGq4f9ZEAuP60oT3vYrqc4PM1on-6oks5YTxQhwA==)

Chat, Play, Socialize.

STOMP and IRC inspired Instant Messaging Client and Server Application.

## Compilation
`make all` to compile both the client and server

`make client` to compile the client

`make server` to compile the server

### Required opam packages

```
lwt v2.5.2
lablgtk v2.18.5
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

##Fun Things to try

####Chatbot room

Join the chatbot room to talk with
a chat bot!

####Easter eggs
Try sending "😶" for something interesting! ;)

