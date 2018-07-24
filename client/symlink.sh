#!/bin/sh
mkdir -p static
ALL_JS=`cabal-plan list-bins | awk '{ print $2 ".jsexe/all.js" }'`
ln -f -s "$ALL_JS" static/all.js
