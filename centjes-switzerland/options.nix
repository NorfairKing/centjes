{ lib }:
{
  ahv-id = lib.mkOption {
    default = null;
    description = "The AHV identifier. e.g. 746.1111.2222.33";
    type = lib.types.nullOr lib.types.str;
  };
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
  domestic-income-account = lib.mkOption {
    default = null;
    description = "Account name of your domestic income";
    type = lib.types.nullOr lib.types.str;
  };
  end = lib.mkOption {
    default = null;
    description = "The final date (inclusive), default: Yesterday";
    type = lib.types.nullOr lib.types.str;
  };
  exports-income-account = lib.mkOption {
    default = null;
    description = "Account name of your exports' income";
    type = lib.types.nullOr lib.types.str;
  };
  first-name = lib.mkOption {
    default = null;
    description = "your first name";
    type = lib.types.nullOr lib.types.str;
  };
  foreign-income-account = lib.mkOption {
    default = null;
    description = "Account name of your foreign income";
    type = lib.types.nullOr lib.types.str;
  };
  last-name = lib.mkOption {
    default = null;
    description = "your last name";
    type = lib.types.nullOr lib.types.str;
  };
  ledger = lib.mkOption {
    default = null;
    description = "ledger file";
    type = lib.types.nullOr lib.types.str;
  };
  organisation-name = lib.mkOption {
    default = null;
    description = "The organisation's legal name";
    type = lib.types.nullOr lib.types.str;
  };
  person-name = lib.mkOption {
    default = null;
    description = "Your legal name";
    type = lib.types.nullOr lib.types.str;
  };
  quarter = lib.mkOption {
    default = null;
    description = "the quarter to produce the report for";
    type = lib.types.nullOr lib.types.unspecified;
  };
  readme-file = lib.mkOption {
    default = null;
    description = "path to the readme file to create";
    type = lib.types.nullOr lib.types.str;
  };
  tag-deductible = lib.mkOption {
    default = null;
    description = "tag to use for deductible purchases";
    type = lib.types.nullOr lib.types.str;
  };
  tag-not-deductible = lib.mkOption {
    default = null;
    description = "tag to use for non-deductible purchases";
    type = lib.types.nullOr lib.types.str;
  };
  vat-expenses-account = lib.mkOption {
    default = null;
    description = "Account name of your the VAT you've paid";
    type = lib.types.nullOr lib.types.str;
  };
  vat-id = lib.mkOption {
    default = null;
    description = "the VAT identifier";
    type = lib.types.nullOr lib.types.str;
  };
  vat-income-account = lib.mkOption {
    default = null;
    description = "Account name of your the VAT you've charged";
    type = lib.types.nullOr lib.types.str;
  };
  year = lib.mkOption {
    default = null;
    description = "the year to produce the report for";
    type = lib.types.nullOr lib.types.int;
  };
  zip-file = lib.mkOption {
    default = null;
    description = "path to the zip file to create";
    type = lib.types.nullOr lib.types.str;
  };
}
