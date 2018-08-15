##################################################
let



in
##################################################

(import ./.).shellFor {
  packages   = p: [p.natlink p.natlink-http];
  withHoogle = true;
}

##################################################
#TODO[rm] (import ./nix/shell.nix)