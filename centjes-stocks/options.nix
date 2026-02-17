{ lib }:
{
  download-rates = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        stocks = lib.mkOption {
          default = null;
          description = "Stock configurations with symbol, ticker, and currency";
          type = lib.types.nullOr (lib.types.listOf (lib.types.submodule {
            options = {
              symbol = lib.mkOption {
                description = "Stock symbol as declared in the ledger (e.g., AAPL)";
                type = lib.types.str;
              };
              ticker = lib.mkOption {
                default = null;
                description = "Ticker symbol for Yahoo Finance API (defaults to symbol, e.g., SWDA.L, BRK-B)";
                type = lib.types.nullOr lib.types.str;
              };
              currency = lib.mkOption {
                description = "Currency the stock is priced in (e.g., USD)";
                type = lib.types.str;
              };
            };
          }));
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
        output = lib.mkOption {
          default = null;
          description = "Output file path (default: stdout)";
          type = lib.types.nullOr lib.types.str;
        };
      };
    };
  };
  ledger = lib.mkOption {
    default = null;
    description = "ledger file";
    type = lib.types.nullOr lib.types.str;
  };
}
