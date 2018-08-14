{

  packageOverrides = pkgs: rec {
    haskellPackages = import ./haskellPackages.nix { inherit pkgs; }; 
  };

}