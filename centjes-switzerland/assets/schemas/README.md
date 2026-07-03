# Notes about XML schemas

There are xsd files on http://ech.ch but they are broken;
* They reference eachother with `http` (not `https`).
* They lack `schemaLocation` attributes sometimes.
* There is no centralised catalog.

Because `xmllint` needs local files; it refuses to download external schemas,
we need to download them ourselves and create a catalog.
The catalog in `catalog.xml` is manually written for validation.

We also had to add some (local) `schemaLocation`s in `eCH-0119-4-0-0.xsd` to get it to compile.
That local fix is tracked as a patch in `nix/schema-patches/eCH-0119-4-0-0.patch`,
applied on top of the pristine upstream file.

## Where the schema files come from

The `.xsd` files themselves are not vendored here. Instead, `nix/schemas.nix`
fetches each one from its upstream as a fixed-output derivation (pinning the
exact bytes by hash), applies the local patch where needed, and assembles them
next to this `catalog.xml`. That directory is exposed as the
`centjesSwitzerlandSchemas` package.

The `centjes-switzerland` executable embeds this directory at compile time via
the `CENTJES_SWITZERLAND_SCHEMA_DIR` environment variable (see
`src/Centjes/Switzerland/Schema.hs`), which both the Nix build and the
development shell set. At runtime the schemas are materialised into a temporary
directory for `xmllint`. The hashes are the provenance: if an upstream file
changes, the build fails on a hash mismatch.

To add, update, or re-patch a schema:

```
curl -fsSL -o pristine.xsd <upstream-url>
nix hash file pristine.xsd                                        # -> sha256 for the manifest
diff -u pristine.xsd patched.xsd > ../../nix/schema-patches/<file>.patch   # only if patched
```

Then add or update the entry in `nix/schemas.nix`.
