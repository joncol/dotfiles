{ sources ? import ./nix/sources.nix,
  pkgs ? (import sources.nixpkgs {})
}:

let
  my-python = pkgs.python38;
  python-with-my-packages = my-python.withPackages (p: with p; [
    selenium
    requests
    python-dateutil
    pillow
    pyquery
    nose2
    pylint
    retrying
  ]);
in
pkgs.mkShell {
  buildInputs = [
    python-with-my-packages
    pkgs.geckodriver
  ];
  shellHook = ''
    PYTHONPATH=${python-with-my-packages}/${python-with-my-packages.sitePackages}
  '';
}
