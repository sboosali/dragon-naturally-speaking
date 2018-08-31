##################################################
{ mkDerivation
}:
##################################################
{ requireFile
}:
##################################################
let

name    = "natlink-${version}-pyd";

version = "4.1victor";

in
##################################################
mkDerivation {

  inherit name version;

  src = fetchurl {
    inherit url sha256;
  };

  builder = requireFile {
    inherit name;
    sha256  = "";
    url     = "";
    message = "";
  };

}
##################################################

# TODO
#
#   requireFile = { name     ? null
#                 , sha256   ? null
#                 , url      ? null
#                 , message  ? null
#                 , hashMode ? "flat"
#                 } :
#
\
##################################################