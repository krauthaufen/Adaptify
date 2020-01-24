@echo off
setlocal enableextensions enabledelayedexpansion
PUSHD %~dp0\bin\Release\netcoreapp2.2

dotnet adaptify.dll --server 



