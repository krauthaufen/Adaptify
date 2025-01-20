@echo off
dotnet tool restore
dotnet paket restore

IF "%1"=="restore" exit /B

@REM Build the tool first, which is used for building the examples.
dotnet build -c Release src\Adaptify.Compiler\adaptify.fsproj
dotnet build -c Release src\Adaptify.sln
