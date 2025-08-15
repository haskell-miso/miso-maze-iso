# miso-maze

Isomorphic web app using [Miso](https://github.com/dmjio/miso) and WebSockets:
move inside the maze and meet other players.

## Build and deploy in a `output` folder:

```
build.sh
```

## Development

```
# build frontend:
nix develop .#wasm --experimental-features "nix-command flakes" --command bash -c "make"

# build backend:
nix develop .#default --experimental-features "nix-command flakes" --command bash -c "cabal build server"

# run tests:
nix develop .#default --experimental-features "nix-command flakes" --command bash -c "cabal test"
```

