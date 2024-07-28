{ lib }:
{
  base-dir = lib.mkOption {
    default = null;
    description = "base directory";
    type = lib.types.nullOr lib.types.str;
  };
  begin = lib.mkOption {
    default = null;
    description = "The begin date (inclusive), default: Start of the year";
    type = lib.types.nullOr lib.types.str;
  };
  end = lib.mkOption {
    default = null;
    description = "The final date (inclusive), default: Yesterday";
    type = lib.types.nullOr lib.types.str;
  };
  ledger = lib.mkOption {
    default = null;
    description = "ledger file";
    type = lib.types.nullOr lib.types.str;
  };
  readme-file = lib.mkOption {
    default = null;
    description = "path to the readme file to create";
    type = lib.types.nullOr lib.types.str;
  };
  zip-file = lib.mkOption {
    default = null;
    description = "path to the zip file to create";
    type = lib.types.nullOr lib.types.str;
  };
}
