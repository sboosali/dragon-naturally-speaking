##################################################
{ pkgs
}:
##################################################
let

generatedOverrides =
 pkgs.haskell.lib.packagesFromDirectory { directory = ../packages; };

manualOverrides = self: super:
  {
    mkDerivation = drv:
      super.mkDerivation (drv //
        {
          #TODO[parametrize] jailbreak   = true;
          
          doHaddock   = true;
          doCheck     = false;
          doBenchmark = false;
          
          enableLibraryProfiling = true;
          
        });
  };

in
##################################################

pkgs.haskellPackages.override
{
  overrides =
    pkgs.lib.composeExtensions generatedOverrides manualOverrides;
}

##################################################