{ lib }:
{
  google-analytics-tracking = lib.mkOption {
    default = null;
    description = "The Google analytics tracking code";
    type = lib.types.nullOr lib.types.str;
  };
  google-search-console-verification = lib.mkOption {
    default = null;
    description = "The Google search console verification code";
    type = lib.types.nullOr lib.types.str;
  };
  port = lib.mkOption {
    default = null;
    description = "port to serve web requests on on";
    type = lib.types.nullOr lib.types.int;
  };
}
