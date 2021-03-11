#!/usr/bin/env bash
set -o errexit -o pipefail -o noclobber -o nounset

# See: https://t.ly/wBrJd for info about `getopt` usage.

! getopt --test > /dev/null
if [[ ${PIPESTATUS[0]} -ne 4 ]]; then
    echo 'I’m sorry, `getopt --test` failed in this environment.'
    exit 1
fi

options=l,x,s,u
longopts=lib,exe,libandexe

! parsed=$(getopt --options=$options --longoptions=$longopts --name "$0" -- "$@")
if [[ ${PIPESTATUS[0]} -ne 0 ]]; then
    exit 2
fi

eval set -- "$parsed"

lib=
exe=true
stable=true

while true; do
    case "$1" in
        -l|--lib)
            lib=true
            shift
            ;;
        -x|--exe)
            exe=true
            shift
            ;;
        --libandexe)
            lib=true
            exe=true
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Programming error $1"
            exit 3
            ;;
    esac
done

if [[ $# -ne 1 ]]; then
    echo "$(basename $0): PROJECT_NAME"
    exit 4
fi

project_dir=$1

mkdir $project_dir
cd $project_dir

target_type=--exe # Default to build executable only.
if [[ $lib = true && $exe = true ]]; then
    echo "Setting up Cabal (library and executable) project..."
    target_type=--libandexe
elif [[ $lib = true ]]; then
    echo "Setting up Cabal (library) project..."
    target_type=--lib
elif [[ $exe = true ]]; then
    echo "Setting up Cabal (executable) project..."
    target_type=--exe
fi

nix-shell --pure -p haskell.compiler.ghc8104 cabal-install \
    --run "cabal init --cabal-version=3.0 -m -l BSD-3-Clause ${target_type} -p ${PWD##*/} \
           -d base -d aeson -d containers -d hashable -d hashmap -d katip \
           -d microlens -d monad-control -d mtl -d text -d transformers-base \
           -d unordered-containers"

nix-shell -p niv --run "niv init -b nixpkgs-unstable"

cat << 'EOF' > default.nix
{ sources ? import ./nix/sources.nix,
  pkgs ? (import sources.nixpkgs {})
}:

let
  t = pkgs.lib.trivial;
  hl = pkgs.haskell.lib;

  pkg = pkgs.haskellPackages.developPackage {
    root = ./.;

    modifier = (t.flip t.pipe)
      [hl.dontHaddock
       hl.enableStaticLibraries
       hl.justStaticExecutables
       hl.disableLibraryProfiling
       hl.disableExecutableProfiling];
  };

in { inherit pkg; }
EOF

cat << EOF > shell.nix
{ sources ? import ./nix/sources.nix,
  pkgs ? (import sources.nixpkgs {})
}:

let
  def = import ./default.nix {};
  dev-pkgs = with pkgs.haskellPackages;
    [ cabal-install
      ghcid
      haskell-language-server
      hlint
      ormolu
    ];

in def.pkg.overrideAttrs (attrs: {
  src = null;
  buildInputs = dev-pkgs ++ attrs.buildInputs;
})
EOF

cat <<'EOF' > .envrc
[[ -n "${DIRENV_ALLOW_NIX}" ]] && use nix

if [ -e .envrc-local ]; then
   source .envrc-local
fi
EOF

direnv allow

gen-hie > hie.yaml
