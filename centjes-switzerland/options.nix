{ lib }:
{
  accident-insurance-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for accident insurance expenses";
    type = lib.types.nullOr lib.types.str;
  };
  ahv-id = lib.mkOption {
    default = null;
    description = "The AHV identifier.";
    type = lib.types.nullOr lib.types.str;
  };
  base-dir = lib.mkOption {
    default = null;
    description = "base directory";
    type = lib.types.nullOr lib.types.str;
  };
  clean = lib.mkOption {
    default = null;
    description = "Clean the packet directory";
    type = lib.types.nullOr lib.types.bool;
  };
  daily-allowance-insurance-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for daily allowance insurance expenses";
    type = lib.types.nullOr lib.types.str;
  };
  daycare-account = lib.mkOption {
    default = null;
    description = "the account to use for daycare expenses";
    type = lib.types.nullOr lib.types.str;
  };
  domestic-income-account = lib.mkOption {
    default = null;
    description = "Account name of your domestic income";
    type = lib.types.nullOr lib.types.str;
  };
  download-rates = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
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
        output = lib.mkOption {
          default = null;
          description = "Where to put the resulting file";
          type = lib.types.nullOr lib.types.str;
        };
      };
    };
  };
  education-account = lib.mkOption {
    default = null;
    description = "the account to use for education expenses";
    type = lib.types.nullOr lib.types.str;
  };
  electricity-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for electricity expenses";
    type = lib.types.nullOr lib.types.str;
  };
  electricity-private-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for private electricity expenses";
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
  health-dentist-account = lib.mkOption {
    default = null;
    description = "the account to use for dentist expenses";
    type = lib.types.nullOr lib.types.str;
  };
  health-doctor-account = lib.mkOption {
    default = null;
    description = "the account to use for doctor and prescription expenses";
    type = lib.types.nullOr lib.types.str;
  };
  health-hospital-account = lib.mkOption {
    default = null;
    description = "the account to use for hospital stay expenses";
    type = lib.types.nullOr lib.types.str;
  };
  health-insurance-premiums-account = lib.mkOption {
    default = null;
    description = "the account to use for health insurance premiums";
    type = lib.types.nullOr lib.types.str;
  };
  health-other-account = lib.mkOption {
    default = null;
    description = "the account to use for other health expenses";
    type = lib.types.nullOr lib.types.str;
  };
  health-therapy-account = lib.mkOption {
    default = null;
    description = "the account to use for therapy and cure expenses";
    type = lib.types.nullOr lib.types.str;
  };
  homeoffice-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for homeoffice expenses";
    type = lib.types.nullOr lib.types.str;
  };
  homeoffice-private-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for private homeoffice expenses";
    type = lib.types.nullOr lib.types.str;
  };
  internet-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for internet expenses";
    type = lib.types.nullOr lib.types.str;
  };
  internet-private-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for private internet expenses";
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
  machinery-assets-account = lib.mkOption {
    default = null;
    description = "the account to use for machinery assets";
    type = lib.types.nullOr lib.types.str;
  };
  machinery-depreciation-rate = lib.mkOption {
    default = null;
    description = "the depreciation rate for machinery (e.g. 0.4 for 40%)";
    type = lib.types.nullOr lib.types.str;
  };
  machinery-expenses-account = lib.mkOption {
    default = null;
    description = "the expenses account for machinery purchases";
    type = lib.types.nullOr lib.types.str;
  };
  movables-assets-account = lib.mkOption {
    default = null;
    description = "the account to use for movables assets";
    type = lib.types.nullOr lib.types.str;
  };
  movables-depreciation-rate = lib.mkOption {
    default = null;
    description = "the depreciation rate for movables (e.g. 0.25 for 25%)";
    type = lib.types.nullOr lib.types.str;
  };
  movables-expenses-account = lib.mkOption {
    default = null;
    description = "the expenses account for movables purchases";
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
  phone-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for phone expenses";
    type = lib.types.nullOr lib.types.str;
  };
  phone-private-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for private phone expenses";
    type = lib.types.nullOr lib.types.str;
  };
  prices = lib.mkOption {
    default = null;
    description = "prices file";
    type = lib.types.nullOr lib.types.str;
  };
  quarter = lib.mkOption {
    default = null;
    description = "the quarter to produce the report for";
    type = lib.types.nullOr lib.types.unspecified;
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
  tag-not-tax-deductible = lib.mkOption {
    default = null;
    description = "tag to use for non-tax-deductible purchases";
    type = lib.types.nullOr lib.types.str;
  };
  tag-not-vat-deductible = lib.mkOption {
    default = null;
    description = "tag to use for non-VAT-deductible purchases";
    type = lib.types.nullOr lib.types.str;
  };
  tag-tax-deductible = lib.mkOption {
    default = null;
    description = "tag to use for tax-deductible purchases";
    type = lib.types.nullOr lib.types.str;
  };
  tag-undeclared = lib.mkOption {
    default = null;
    description = "tag to use for undeclared asset accounts";
    type = lib.types.nullOr lib.types.str;
  };
  tag-vat-deductible = lib.mkOption {
    default = null;
    description = "tag to use for VAT-deductible purchases";
    type = lib.types.nullOr lib.types.str;
  };
  taxes = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        packet-dir = lib.mkOption {
          default = null;
          description = "Path to the packet directory to create";
          type = lib.types.nullOr lib.types.str;
        };
        zip-file = lib.mkOption {
          default = null;
          description = "Path to the zip file to create";
          type = lib.types.nullOr lib.types.str;
        };
      };
    };
  };
  third-pillar-assets-account = lib.mkOption {
    default = null;
    description = "the account to use for third pillar assets";
    type = lib.types.nullOr lib.types.str;
  };
  third-pillar-insurance-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for third pillar insurance expenses";
    type = lib.types.nullOr lib.types.str;
  };
  travel-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for travel expenses";
    type = lib.types.nullOr lib.types.str;
  };
  travel-private-expenses-account = lib.mkOption {
    default = null;
    description = "the account to use for private travel expenses";
    type = lib.types.nullOr lib.types.str;
  };
  vat = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        packet-dir = lib.mkOption {
          default = null;
          description = "Path to the packet directory to create";
          type = lib.types.nullOr lib.types.str;
        };
        zip-file = lib.mkOption {
          default = null;
          description = "path to the zip file to create";
          type = lib.types.nullOr lib.types.str;
        };
      };
    };
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
  watch = lib.mkOption {
    default = null;
    description = "Run centjes in a loop";
    type = lib.types.nullOr lib.types.bool;
  };
  year = lib.mkOption {
    default = null;
    description = "the year to produce the report for";
    type = lib.types.nullOr lib.types.int;
  };
}
