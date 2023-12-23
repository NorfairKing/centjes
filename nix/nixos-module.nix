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
      enable = mkEnableOption (mdDoc "Centjes Service");
      docs-site = mkOption {
        default = null;
        description = mdDoc "Centjes' documentation site service";
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption (mdDoc "Centjes Docs Site");
            config = mkOption {
              description = mdDoc "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
              type = types.attrs;
              default = { };
            };
            port = mkOption {
              description = mdDoc "The port to serve sync requests on";
              type = types.int;
              example = 8000;
            };
            hosts = mkOption {
              description = mdDoc "The host to serve the docs site on";
              type = types.listOf types.str;
              default = [ ];
              example = [ "docs.centjes.online" ];
            };
            google-analytics-tracking = mkOption {
              description = mdDoc "The Google analytics tracking code";
              type = types.nullOr types.str;
              example = "XX-XXXXXXXX-XX";
              default = null;
            };
            google-search-console-verification = mkOption {
              description = mdDoc "The Google search console verification code";
              type = types.nullOr types.str;
              example = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
              default = null;
            };
            pkg = mkOption {
              description = mdDoc "The docs site package";
              type = types.package;
              default = centjes-docs-site;
            };
          };
        });
      };
    };
  config =
    let
      working-dir = "/www/centjes/${envname}/";
      attrOrNull = name: value: optionalAttrs (!builtins.isNull value) { "${name}" = value; };
      # The docs server
      docs-site-config = with cfg.docs-site; mergeListRecursively [
        (attrOrNull "port" port)
        (attrOrNull "google-analytics-tracking" google-analytics-tracking)
        (attrOrNull "google-search-console-verification" google-search-console-verification)
        cfg.docs-site.config
      ];
      docsSiteConfigFile = (pkgs.formats.yaml { }).generate "centjes-docs-site-config.yaml" docs-site-config;
      docs-site-service =
        optionalAttrs (cfg.docs-site.enable or false) {
          "centjes-docs-site-${envname}" =
            with cfg.docs-site;
            {
              description = "Centjes docs site ${envname} Service";
              wantedBy = [ "multi-user.target" ];
              environment =
                {
                  "CENTJES_DOCS_SITE_CONFIG_FILE" = "${docsSiteConfigFile}";
                };
              script =
                ''
                  ${pkg}/bin/centjes-docs-site
                '';
              serviceConfig =
                {
                  Restart = "always";
                  RestartSec = 1;
                  Nice = 15;
                };
              unitConfig =
                {
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
      networking.firewall.allowedTCPPorts = (optional (cfg.docs-site.enable or false) cfg.docs-site.port);
      services.nginx.virtualHosts = docs-site-host;
    };
}
