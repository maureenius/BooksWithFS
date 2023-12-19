FROM mcr.microsoft.com/dotnet/runtime:7.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:7.0 AS build
WORKDIR /src
COPY ["CodingameFallChallenge2023/CodingameFallChallenge2023.fsproj", "CodingameFallChallenge2023/"]
RUN dotnet restore "CodingameFallChallenge2023/CodingameFallChallenge2023.fsproj"
COPY . .
WORKDIR "/src/CodingameFallChallenge2023"
RUN dotnet build "CodingameFallChallenge2023.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "CodingameFallChallenge2023.fsproj" -c Release -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "CodingameFallChallenge2023.dll"]
