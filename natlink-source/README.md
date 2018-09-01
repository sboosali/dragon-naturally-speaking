# `NatLink`installer

## binary installer

the `.exe` is the first stable release of a `NatLink` that is compatible with `Dragon-NaturallySpeaking v15` (circa mid-2018). 

## source code

later, the NatLink Project should release the source officially, including the fixed `natlink.pyd`. now, i just ran the `.exe` (a "self-extracting installer") on a Windows machine and copied the extracted files here. see `./ Natlink-4.1-victor/`.

### licensing

**NOTE** the `natlink.pyd` file has a "must-download-manually" license (like `VirtualBox guest additions`).

## origin

    $ wget https://downloads.sourceforge.net/project/natlink/natlink/natlinktest4.1/setup-natlink-4.1victor.exe
    
    $ nix-prefetch-url https://downloads.sourceforge.net/project/natlink/natlink/natlinktest4.1/setup-natlink-4.1victor.exe

## 