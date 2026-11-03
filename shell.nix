{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.11.tar.gz";
    sha256 = "1gx0hihb7kcddv5h0k7dysp2xhf1ny0aalxhjbpj2lmvj7h9g80a";
  }) {} 
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    R
    rPackages.renv
    rPackages.V8
    rPackages.dplyr
    rPackages.lubridate
    rPackages.ical
    rPackages.httr2
    rPackages.stringr
    rPackages.glue
    rPackages.jsonlite
    rPackages.ggplot2
    rPackages.knitr
    openssl
    curl
    pkg-config
    nodejs
    zlib
  ];

  shellHook = ''
    export LANG=C.UTF-8
    export LC_ALL=C.UTF-8
    echo "Nix environment loaded with clean UTF-8 locales!"
  '';
}
