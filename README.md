# Lobby
List active game servers and allow for the creation of new rooms.

## Compile
The Elm code has already been compiled into the Javascript that is used by `Lobby.html`, but if additional changes are made, the source can be recompiled with

```elm make src/Lobby.elm --output=assets/js/main.js && elm reactor```