:: Build
dotnet restore
dotnet build -c Release FsiRefGen.sln

:: Pack
.paket\paket pack . --template FsiRefGen.paket.template