{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "schminke";
  buildInputs = [ llvm_4 bats ];
}
