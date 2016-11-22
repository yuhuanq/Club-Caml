- Client
send user name in headers of CONNECT message
Use lwt to implement
separate functions during connection
and for when connected
leave and enter a room by using subscribe and unsubscribe
NOTTY as a GUI kinda thing
Or just implement as similar to utop

-Server
data structure for (username*output channel) pair as value and destination as key
Send back to client the list of active rooms in three cases:
  a. When a client connects (i.e. in the "CONNECTED" message)
  b. When a client unsubscribes (i.e in the response to unsubscribe)
  c. If error occurs from subscribe request (i.e in headers of error)

-Testing
testing file for client, for server
and then testing together