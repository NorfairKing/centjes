{ lib }:
{
  balance = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        convert = lib.mkOption {
          default = null;
          description = "Currency to convert to";
          type = lib.types.nullOr lib.types.str;
        };
        show-empty = lib.mkOption {
          default = null;
          description = "Show empty balances instead of hiding them";
          type = lib.types.nullOr lib.types.bool;
        };
        virtual = lib.mkOption {
          default = null;
          description = "Show virtual postings too";
          type = lib.types.nullOr lib.types.bool;
        };
        year = lib.mkOption {
          default = null;
          description = "Balance at the end of the given year";
          type = lib.types.nullOr lib.types.int;
        };
      };
    };
  };
  ledger = lib.mkOption {
    default = null;
    description = "ledger file";
    type = lib.types.nullOr lib.types.str;
  };
  log-level = lib.mkOption {
    default = null;
    description = "Minimal severity of log messages";
    type = lib.types.nullOr (lib.types.enum [
      "Debug"
      "Info"
      "Warn"
      "Error"
    ]);
  };
  register = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        convert = lib.mkOption {
          default = null;
          description = "Currency to convert to";
          type = lib.types.nullOr lib.types.str;
        };
        virtual = lib.mkOption {
          default = null;
          description = "Show virtual postings too";
          type = lib.types.nullOr lib.types.bool;
        };
        year = lib.mkOption {
          default = null;
          description = "Balance at the end of the given year";
          type = lib.types.nullOr lib.types.int;
        };
      };
    };
  };
  watch = lib.mkOption {
    default = null;
    description = "Run centjes in a loop";
    type = lib.types.nullOr lib.types.bool;
  };
}
