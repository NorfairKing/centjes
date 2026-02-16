# Centjes

Plaintext double-entry accounting system focused on precision and safety.

## Project Structure

Multi-package Haskell monorepo:

- `centjes/` - Main CLI application (parser, ledger, reports)
- `centjes-switzerland/` - Swiss tax and VAT report generation
- `centjes-import-{cornercard,neon,revolut}/` - Bank statement importers
- `centjes-cryptocurrencies/` - Crypto rate downloads
- `centjes-gen/` - Generators and tests
- `centjes-docs-site/` - Yesod documentation website
- `centjes-vim/` - Vim syntax highlighting for `.cent` files

## Build & Test

Uses Nix flakes + Stack:

```bash
# Enter dev environment
nix develop

# Build
stack build <package>

# Test
stack test <package>

# Start golden output
stack test --test-arguments "--golden-start"

# Reset golden output
stack test --test-arguments "--golden-reset"
```

## Claude-specific

* Don't use abbreviations if possible. Especially in field name prefixes.

## CLI Commands

Main `centjes` executable:
- `check` - Validate ledger
- `balance` - Generate balance reports
- `register` - Transaction register
- `format` - Format ledger files
- `rates-graph` - Currency conversion graph

## Parser

Uses Alex (lexer) + Happy (parser) for `.cent` file format. Located in `centjes/src/Centjes/Parse/`.

## Key Libraries

- `safe-coloured-text` - Colored terminal output
- `really-safe-money` - Precise monetary calculations
- `sydtest` - Test framework
- `opt-env-conf` - CLI/env configuration
- `yesod` - Docs site web framework

## File Format

Accounting files use `.cent` extension with custom syntax for transactions, accounts, currencies, and assertions.
