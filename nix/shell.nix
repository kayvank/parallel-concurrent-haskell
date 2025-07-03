# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectinshellargs
# See `shellArgs` in `mkHaskellProject` in ./project.nix for more details.

{ repoRoot, inputs, pkgs, lib, system }:

# Each flake variant defined in your project.nix project will yield a separate
# shell. If no flake variants are defined, then cabalProject is the original 
# project.
cabalProject: {
  name = "kayvan-sandbox";

  # prompt = null;

  # welcomeMessage = null;

  packages = [ pkgs.ghcid pkgs.librsvg pkgs.haskellPackages.threadscope ];

  # scripts = {
  #   foo = {
  #      description = "";
  #      group = "general";
  #      enabled = true;
  #      exec = ''
  #        echo "Hello, World!"
  #      '';
  #    };
  # };

  # env = {};

  shellHook = ''
    set -o vi
    export CABAL_DIR="$(pwd)/.cabal"
  '';

  tools = {
    # haskellCompilerVersion = cabalProject.args.compiler-nix-name;
    cabal-fmt = null;
    # cabal-install = null;
    haskell-language-server = null;
    haskell-language-server-wrapper = null;
    fourmolu = null;
    hlint = null;
    # stylish-haskell = null;
    ghcid = null;
    # shellcheck = null;
    # prettier = null;
    # editorconfig-checker = null;
    nixfmt-classic = null;
    # optipng = null;
    # purs-tidy = null;
  };

  preCommit = {
    cabal-fmt.enable = true;
    #   cabal-fmt.extraOptions = "";
    #   stylish-haskell.enable = false;
    #   stylish-haskell.extraOptions = "";
    fourmolu.enable = true;
    #   fourmolu.extraOptions = "";
    #   hlint.enable = false;
    #   hlint.extraOptions = "";
    #   shellcheck.enable = false;
    #   shellcheck.extraOptions = "";
    #   prettier.enable = false;
    #   prettier.extraOptions = "";
    #   editorconfig-checker.enable = false;
    #   editorconfig-checker.extraOptions = "";
    nixfmt-classic.enable = true;
    #   nixfmt-classic.extraOptions = "";
    #   optipng.enable = false;
    #   optipng.extraOptions = "";
    #   purs-tidy.enable = false;
    #   purs-tidy.extraOptions = "";
  };
}
