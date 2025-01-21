@echo off
dotnet tool restore
dotnet paket restore

IF "%1"=="restore" exit /B

@REM Build the MSBuild tool first, which is used for building the examples.
dotnet build -c Release src\Adaptify.MSBuild\Adaptify.MSBuild.fsproj
dotnet build -c Release src\Adaptify.sln
