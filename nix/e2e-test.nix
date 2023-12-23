{ nixosTest
, system
, centjes-nixos-module
}:
nixosTest ({ lib, pkgs, ... }:
with lib;
let
  docs-port = 8001;
in
{
  name = "centjes-e2e-test";
  nodes = {
    docsserver = {
      imports = [
        centjes-nixos-module
      ];
      system.stateVersion = "23.11";
      services.centjes.production.docs-site = {
        enable = true;
        docs-site = {
          enable = true;
          port = docs-port;
        };
      };
    };
    client = { config, ... }: {
      system.stateVersion = "23.11";
    };
  };
  testScript = ''
    docsserver.start()
    client.start()

    docsserver.wait_for_unit("default.target")
    client.wait_for_unit("default.target")

    docsserver.wait_for_open_port(${builtins.toString docs-port})
    client.succeed("curl docsserver:${builtins.toString docs-port}")
  '';
})
