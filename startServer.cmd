@echo off
setlocal enableextensions enabledelayedexpansion
PUSHD %~dp0\bin\Release\netcoreapp2.2

dotnet adaptify.dll --killserver
dotnet adaptify.dll --server --verbose



