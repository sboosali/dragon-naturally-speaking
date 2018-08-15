##################################################
{ pkgs
}:
##################################################
let

generatedOverrides =
 pkgs.haskell.lib.packagesFromDirectory { directory = ../packages; };

manualOverrides = self: super:

  ####################
  let
  haskellPackageOverrides =
    (import ../overrides/haskell self super);

  mkDerivationOverrides = {
    mkDerivation = drv:
      super.mkDerivation (drv //
        {
          #TODO[parametrize] jailbreak   = true;
          
          #doHaddock   = true;
          doCheck     = false;
          doBenchmark = false;
          
          enableLibraryProfiling = true;
          
        });
  };
  in
  ####################
  
  haskellPackageOverrides // mkDerivationOverrides;

overrides =
    pkgs.lib.composeExtensions generatedOverrides manualOverrides;

in
##################################################

pkgs.haskellPackages.override
{
  overrides = overrides;
}

##################################################