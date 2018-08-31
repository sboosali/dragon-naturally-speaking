##################################################
{ mkDerivation
}:
##################################################
{ fetchurl
, natlink-pyd
}:
##################################################
let

name    = "natlink-${version}";

version = "4.1victor";

in
##################################################
let

url    = "https://downloads.sourceforge.net/project/natlink/natlink/natlink-${version}.tag.gz";

sha256 = "";

in
##################################################
mkDerivation {

  inherit name version;

  src = fetchurl {
    inherit url sha256;
  };

  buildDepends = [
    natlink-pyd
  ];

  # builder = ''
  # '';

}
##################################################