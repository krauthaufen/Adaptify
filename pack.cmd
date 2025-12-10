@echo off
CALL build.cmd

dotnet publish .\src\Adaptify.MSBuild\Adaptify.MSBuild.fsproj -c Release -o bin/publish/msbuild-tool
dotnet aardpack --skip-build --notag .\src\Adaptify.sln

REM cannot use --no-build for dotnet pack due to https://github.com/dotnet/fsharp/issues/12320
FOR /F "tokens=*" %%g IN ('dotnet aardpack --parse-only') do (SET VERSION=%%g)
dotnet pack .\src\adaptify\adaptify.fsproj -c Release -p:PackageVersion=%VERSION% -o bin\pack