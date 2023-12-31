{ pkgs, ... }:

{
  services.clickhouse.enable = true;

  # Enable the HTTP iterface
  services.clickhouse.config = ''
    http_port: 8123
    listen_host: '::'
  '';

  packages = [
    pkgs.zlib
  ];
}
