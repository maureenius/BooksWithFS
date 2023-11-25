FROM mcr.microsoft.com/dotnet/runtime:7.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:7.0 AS build
WORKDIR /src
COPY ["tansaku/tansaku.fsproj", "tansaku/"]
RUN dotnet restore "tansaku/tansaku.fsproj"
COPY . .
WORKDIR "/src/tansaku"
RUN dotnet build "tansaku.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "tansaku.fsproj" -c Release -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "tansaku.dll"]
