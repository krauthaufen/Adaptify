#!/bin/bash

if [ ! -f .paket/paket ]; then
    echo installing paket
    dotnet tool install Paket --tool-path .paket
fi

dotnet tool restore
dotnet paket restore
dotnet paket generate-load-scripts -g build
dotnet fsi build.fsx $@ 