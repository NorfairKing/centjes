{ nixosTest
, system
, centjes-nixos-module
}:
let
  docs-port = 8001;
in
nixosTest {
  name = "centjes-e2e-test";
  nodes = {
    docsserver = {
      imports = [
        centjes-nixos-module
      ];
      system.stateVersion = "24.05";
      services.centjes.production = {
        enable = true;
        docs-site = {
          enable = true;
          config = {
            port = docs-port;
          };
        };
      };
    };
    client = {
      system.stateVersion = "24.05";
    };
  };
  testScript = ''
    docsserver.start()
    client.start()

    docsserver.wait_for_unit("default.target")
    client.wait_for_unit("default.target")

    docsserver.systemctl("status centjes-docs-site-production.service")
    docsserver.wait_for_unit("centjes-docs-site-production.service")
    docsserver.wait_for_unit("default.target")

    docsserver.wait_for_open_port(${builtins.toString docs-port})
    client.succeed("curl docsserver:${builtins.toString docs-port}")
  '';
}
