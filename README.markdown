TPut
=====

A trivial file upload service.

The use case I have in mind is as a replacement for Pastebin when developing
for ComputerCraft. Just launch an instance of TPut and use the `tp` and `tg`
commands instead of `pastebin put` and `pastebin get`.

No more annoying Pastebin codes.

No more Pastebin upload limits.

Just files.

Setup
=====

You know the drill:

1. `cabal sandbox init`
2. `cabal install --only-dependencies`
3. `cabal build`

Then make an uploads directory and serve files from it.

1. `mkdir data`
2. `cabal run data`

The service listens on port 8080.

If you don't like this:

* then change the code to use a different port
* send a pull request with something to use environment variables
* find yet another Pastebin alternative; there are plenty enough as it is

Usage
=====

These are the available endpoints.

* `GET /files/<filename>` retrieves the contents of _filename_ or 404s.
* `POST /files/<filename>` creates or overwrites the contents of _filename_
  with the request body.
* `GET /list` retrieves a list of files stored on the server with one file name
  per line.

Servant will automatically deal with issues like malicious users injecting dots
or slashes into the URL.

Ensure that the `Content-Type` header in your request is set to
`text/plain;charset=utf-8`. Anything else will upset servant because I'm too
lazy to make it more lenient. In particular HTTP 415 responses will be
produced.
