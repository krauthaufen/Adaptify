#!/bin/bash

dotnet tool restore
dotnet paket restore
dotnet paket generate-load-scripts -g build
dotnet fsi build.fsx $@ 