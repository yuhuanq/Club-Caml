
IMPORTANT: READ THIS

Game frames required headers:
1. destination (current room)
2. Opponent (required only for initial challenge, optional for subsequent frames )

- Game frame bodies MUST be valid game commands otherwise an ERROR frame will be
returned

- Server will send back a GAME_MESSAGE frame to ALL subscribes in the current
topic. The body will be a string representation of the current state of the
game. Display this.

