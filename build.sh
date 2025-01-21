#!/bin/bash

dotnet tool restore
dotnet paket restore
# Build the MSBuild tool first, which is used for building the examples.
dotnet build -c Release src/Adaptify.MSBuild/Adaptify.MSBuild.fsproj
dotnet build -c Release src/Adaptify.NonWindows.slnf