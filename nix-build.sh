#!/bin/bash
BIN=apie-cgi
DEST_PATH=$(pwd)/cgi-bin/apie.cgi
BUILD_SCRIPT=$(nix-build --no-link -A fullBuildScript)
NIX_PATH=$($BUILD_SCRIPT)/bin/$BIN
cp $NIX_PATH $DEST_PATH
if [ $? -eq 0 ]
then
  chmod 750 $DEST_PATH
  echo "Copied $BIN to $DEST_PATH."
elif [ -f $NIX_PATH ]
then
  echo "You can find your binary in $NIX_PATH."
else
  echo "Could not compile $BIN."
fi
