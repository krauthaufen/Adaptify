#!/bin/bash

dotnet tool restore
dotnet paket restore
# Build the tool first, which is used for building the examples.
dotnet build -c Release src/Adaptify.Compiler/adaptify.fsproj
dotnet build -c Release src/Adaptify.NonWindows.slnf