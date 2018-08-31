##################################################
{ mkDerivation
}:
##################################################
{ fetchurl
}:
##################################################
let

name    = "natlink-${version}";

version = "4.1victor";

in
##################################################
let

url    = "https://downloads.sourceforge.net/project/natlink/natlink/natlinktest4.1/setup-natlink-${version}.exe";

sha256 = "1k2whnwpcqy0b4wjd17kwljivqd0srrli6p55xkk5hrpg2k2l806";

in
##################################################
mkDerivation {

  inherit name version;

  src = fetchurl {
    inherit url sha256;
  };

  #TODO
  builder = ''
  mkdir -p $out/bin
  mv $src $out/bin
  '';
  # ^ we just copy the `.exe`.

}
##################################################