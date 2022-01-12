@echo off
setlocal enableextensions enabledelayedexpansion
PUSHD %~dp0

dotnet tool restore
dotnet paket restore
dotnet paket generate-load-scripts -g build
REM dotnet fsi --compilertool:./packages/build/FSharp.DependencyManager.Paket/lib/netstandard2.0 build.fsx
dotnet fsi build.fsx %*

