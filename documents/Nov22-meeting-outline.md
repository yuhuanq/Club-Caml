Meeting Schedule
3110 Implementation Phase
Nov 22, 2016

<!-- From 11/23 to 11/27 -->
<!-- daily impl meetings @ 12pm -->

1. Go over protocol with everyone

2. Figure out what parts of server/client to implement first
    - flush out more of the mli files for both
    - test_client.ml
    - test_server.ml

0. Protocol
    - Basically Done
    - could use some more testing though

1. Server
    - read/send using Protocol
    - val handle_subscribe
    - val handle_unsubscribe
    - val handle_send
        If the sever cannot successfully process the SEND frame frame for any reason,
        the server MUST send the client an ERROR frame and disconnect the client.
    - val handle_disconnect
    - val greeting (send to client when first connected)
    - data structure for keeping track of subscriptions: Dict with k's as
      destinations/topics and values being the subscribers (by oc or uniqueID?)

2. Client
    - Notty
    - read/send using Protocol
    - initial connection phase
    - methods: while connected phase
        - val handle_connected
        - val handle_message
        - val handle_error
    - user interface
    - on unsubscribe have server send new frame with data on the current active
      rooms
    - if Error frame on nonexisting subscribing to a room, then server will send
      active reooms


