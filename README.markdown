Atuin
=====

Atuin is the name of the codename of the Turtle Empire project. Atuin seeks to
create a self-sustaining and autonomous community of Minecraft turtles and
computers.

The overall structure of the system is centralized. A central manager has an
internal world representation that is continuously updated as turtles perform
side-effects on the world. The central manager runs planning algorithm on the
current world state to determine which action needs to be performed next. An
idle turtle is selected to perform the action.

In practice, the software loaded onto each individual turtle is minimal. In
their idle state, turtles issue a request for work to the central computer.
This request blocks until the central computer finds work for the turtle. The
work is transferred to the turtle as a string representation of a Lua program
that is executed by the turtle.

The central manager generates Lua programs by using a template-based system. In
the future, an EDSL may be developed to create more sophisticated Lua programs.

TPut
-----

The Atuin server provides functionality that is a strict superset of the
functionality of TPut, from which it is forked. That being said, files can be
uploaded and downloaded from the Atuin server using `POST` and `GET` requests
under the `files` route.

Setup
=====

You know the drill:

1. `cabal sandbox init`
2. `cabal install --only-dependencies`
3. `cabal build`

To use the tput subsystem of the Atuin server, an uploads directory is
required.

1. `mkdir data`
2. `cabal run data`

The service listens on port 8080.

If you don't like this:

* then change the code to use a different port
* send a pull request with something to use environment variables

Usage
=====

These are the available endpoints for the TPut subsystem.

* `GET /files/:path` retrieves the contents of _path_ or 404s.
* `POST /files/:path` creates or overwrites the contents of _path_
  with the request body.
* `GET /list` retrieves a list of files stored on the server with one file name
  per line.

Servant will automatically deal with issues like malicious users injecting dots
into the URL. Slashes may be used in the capture parameter in order to place
files into subdirectories. However, there is currently no support for creating
directories, so attempting to place a file into a subdirectory that does not
exist will fail.

Ensure that the `Content-Type` header in your request is set to
`text/plain;charset=utf-8`. Anything else will upset servant because I'm too
lazy to make it more lenient. In particular HTTP 415 responses will be
produced.

Atuin services
--------------

### Messaging

Atuin provides the `/msg` route for sending messages between computers within
minecraft by proxying the message through the Atuin server. By using this proxy
scheme, we avoid the need to collect Ender Pearls, required in order to craft
wireless modems. This allows communication between any two computers across
arbitrary distances and even across dimensions or even across servers.
Furthermore, message delivery can be delayed; the messages are recorded
server-side in queue when sent, so the destination computer need not be online
in order to eventually receive the message.

These are the endpoints associated with the messaging subsystem.

* `GET /msg/:computerId` retrieves a message destined for the given
  _computerId_.
* `POST /msg/:computerId` sends a message to the given _computerId_.

Message receiving via `GET /msg/:computerId` is nonblocking: if there are no
available messages for _computerId_, the then server will produce a response
with no body. Consequently, sending empty strings is not possible. We have
found that sending empty strings is not useful enough for this to be a problem.

### Blockdata

The blockdata endpoint is used to signal new information about a block to the
central server.

Every time a turtle moves, it should intelligently inspect up, down, and
forward to determine the type of blocks around it and send this information to
the central server. This information is used to update the central server's
internal world representation.

* `POST /blockdata` updates the information about the block at a given position
  in the internal world representation.

  Required query string parameters:

  * `at`: type `int`, format comma-separated.
