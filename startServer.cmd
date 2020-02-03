@echo off
setlocal enableextensions enabledelayedexpansion
PUSHD %~dp0\bin\Release\netcoreapp3.0

dotnet adaptify.dll --killserver
dotnet adaptify.dll --server --verbose



