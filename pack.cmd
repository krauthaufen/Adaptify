@echo off
CALL build.cmd

dotnet publish .\src\Adaptify.MSBuild\Adaptify.MSBuild.fsproj -c Release -o bin/publish/msbuild-tool
dotnet aardpack --skip-build --notag .\src\Adaptify.sln

FOR /F "tokens=*" %%g IN ('dotnet aardpack --parse-only') do (SET VERSION=%%g)
dotnet pack .\src\adaptify\adaptify.fsproj --no-build -c Release -p:PackageVersion=%VERSION% -o bin\pack