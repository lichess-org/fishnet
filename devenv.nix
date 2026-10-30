{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

let
  cdFishnet = ''
    if [ -f "$DEVENV_ROOT/src/stockfish.rs" ]; then
      cd "$DEVENV_ROOT"
    else
      dir="''${FISHNET_PATH:-$DEVENV_ROOT/../fishnet}"
      cd "$dir" || { echo "fishnet checkout not found at $dir; clone it there or set FISHNET_PATH" >&2; exit 1; }
    fi
  '';
in
{
  languages.rust.enable = true;

  processes = {
    fishnet-play = {
      exec = ''
        ${cdFishnet}
        exec cargo run -- \
          --endpoint http://localhost:9665/fishnet \
          --key fishnetplaydev \
          --max-backoff 5
      '';
    };

    fishnet-analyze = {
      exec = ''
        ${cdFishnet}
        exec cargo run -- \
          --endpoint http://localhost:9663/fishnet \
          --max-backoff 5
      '';
    };
  };
}
