##################################################
{ nixpkgs ? import <nixpkgs> {}
}:
##################################################

nixpkgs.python27.withPackages (ps: with ps;
 [ requests
 ])

##################################################
# Notes
# =====

# Python-2.7
#
#    nixrepl> :i python27
#    installing 'python-2.7.15.drv'
#
#    nixrepl> :i python27Full
#      /nix/store/*-python2.7-requests-2.19.1
#      /nix/store/*-python2.7-requests-2.19.1-dev
#
#    nixrepl> :i python2nix
#    

##################################################