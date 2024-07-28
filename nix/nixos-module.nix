{ centjes-docs-site
}:
{ envname
}:
{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.centjes."${envname}";

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };
in
{
  options.services.centjes."${envname}" =
    {
      enable = mkEnableOption ("Centjes Service");
      docs-site = mkOption {
        default = null;
        description = "Centjes' documentation site service";
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Centjes Docs Site";
            pkg = mkOption {
              description = "The docs site package";
              type = types.package;
              default = centjes-docs-site;
            };
            hosts = mkOption {
              description = "The host to serve the docs site on";
              type = types.listOf types.str;
              default = [ ];
              example = [ "docs.centjes.online" ];
            };
            config = mkOption {
              description = "Typed settings passed by config file";
              default = { };
              type = types.submodule {
                options = import ../centjes-docs-site/options.nix { inherit lib; };
              };
            };
            extraConfig = mkOption {
              description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
              default = { };
            };
          };
        });
      };
    };
  config =
    let
      # The docs server
      docs-site-config = mergeListRecursively [
        (builtins.removeAttrs cfg.docs-site.config [ "override" "overrideDerivation" ])
        cfg.docs-site.extraConfig
      ];
      docsSiteConfigFile = (pkgs.formats.yaml { }).generate "centjes-docs-site-config.yaml" docs-site-config;
      docs-site-service =
        optionalAttrs (cfg.docs-site.enable or false) {
          "centjes-docs-site-${envname}" =
            with cfg.docs-site;
            {
              description = "Centjes docs site ${envname} Service";
              wantedBy = [ "multi-user.target" ];
              environment = {
                "CENTJES_DOCS_SITE_CONFIG_FILE" = "${docsSiteConfigFile}";
              };
              script = ''
                ${pkg}/bin/centjes-docs-site
              '';
              serviceConfig = {
                Restart = "always";
                RestartSec = 1;
                Nice = 15;
              };
              unitConfig = {
                StartLimitIntervalSec = 0;
                # ensure Restart=always is always honoured
              };
            };
        };
      docs-site-host =
        optionalAttrs ((cfg.docs-site.enable or false) && (cfg.docs-site.hosts or [ ]) != [ ]) {
          "${head cfg.docs-site.hosts}" =
            with cfg.docs-site;
            {
              enableACME = true;
              forceSSL = true;
              locations."/".proxyPass =
                "http://localhost:${builtins.toString port}";
              serverAliases = tail hosts;
            };
        };
    in
    mkIf (cfg.enable or false) {
      systemd.services = docs-site-service;
      networking.firewall.allowedTCPPorts = (optional (cfg.docs-site.enable or false) cfg.docs-site.config.port);
      services.nginx.virtualHosts = docs-site-host;
    };
}
