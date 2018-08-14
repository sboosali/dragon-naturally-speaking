{

  packageOverrides = pkgs: rec {
    haskellPackages = import ./haskellPackages.nix pkgs;
  };

}