# The directory of XML schemas that the centjes-switzerland executable embeds
# at compile time (via CENTJES_SWITZERLAND_SCHEMA_DIR + file-embed).
#
# Instead of vendoring the XSDs in git, we fetch each one from its upstream as a
# fixed-output derivation (so its hash pins the exact bytes), apply a tracked
# local patch where needed, and assemble them next to the hand-written
# catalog.xml. The hashes are the provenance: if upstream changes, the build
# fails on a hash mismatch.
#
# To add, update, or re-patch a schema:
#   curl -fsSL -o pristine.xsd <url>
#   nix hash file pristine.xsd                                  # -> sha256
#   diff -u pristine.xsd patched.xsd > schema-patches/<file>.patch   # if patched
{ lib, runCommandLocal, fetchurl }:
let
  catalog = ../centjes-switzerland/assets/schemas/catalog.xml;

  # Provenance of each schema.
  #   url    - upstream source
  #   sha256 - SRI hash of the *pristine upstream* bytes
  #   patch  - optional local fix applied on top (see assets/schemas/README.md)
  schemas = {
    "eCH-0006-2-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0006/2/eCH-0006-2-0.xsd";
      sha256 = "sha256-dLNbTBcmK3gpVfL0p+sWhb3wFEkkqxJGGq3wi0iSevY=";
    };
    "eCH-0007-5-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0007/5/eCH-0007-5-0.xsd";
      sha256 = "sha256-1ufBJcZFPLCDEp4SyPMTy++obHp/uWfjTabJikDWX6A=";
    };
    "eCH-0007-5-0f.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0007-f/5/eCH-0007-5-0f.xsd";
      sha256 = "sha256-5x0swgAMCktMLsqhB7i3ldRjYCYPVcqk0Au+zRDhqG4=";
    };
    "eCH-0007-6-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0007/6/eCH-0007-6-0.xsd";
      sha256 = "sha256-U7nan/Flf5BzKOIkzfLJGYUyQH0VaDZU8K63chvcisg=";
    };
    "eCH-0007-6-0f.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0007-f/6/eCH-0007-6-0f.xsd";
      sha256 = "sha256-CrfQyOTugtmssQhL+6gX453h5Ck3fML4ecyHshaQA8Q=";
    };
    "eCH-0008-3-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0008/3/eCH-0008-3-0.xsd";
      sha256 = "sha256-TfXeeolGkPJAbiPcfuFj+dpZ2thYpdZXzOfbao2H9G8=";
    };
    "eCH-0008-3-0f.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0008-f/3/eCH-0008-3-0f.xsd";
      sha256 = "sha256-26zsd/jUkPTjQZYr/+nZO68rvPZqIdR30wr+tc8lYJY=";
    };
    "eCH-0010-5-1f.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0010-f/5/eCH-0010-5-1f.xsd";
      sha256 = "sha256-x0GaSGLcMvSjE57py1d2BJIeHecaWO5Vlo50IE4ASxE=";
    };
    "eCH-0010-6-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0010/6/eCH-0010-6-0.xsd";
      sha256 = "sha256-hCh1/cRv4AsnmLlP9Vpda2fqmT5eQUtgfLpp9fAxb9g=";
    };
    "eCH-0010-7-0f.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0010-f/7/eCH-0010-7-0f.xsd";
      sha256 = "sha256-mpDsvqpMv6jhn9SObL5czvXfpN184zH3BNBb13Oz/mk=";
    };
    "eCH-0010-8-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0010/8/eCH-0010-8-0.xsd";
      sha256 = "sha256-x/O4DoLrsooNgssAw8j4rqrqpRAbzDPfRgtUmlayKXw=";
    };
    "eCH-0011-8-0f.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0011-f/8/eCH-0011-8-0f.xsd";
      sha256 = "sha256-fDr6UrHnTIAqQerVoFZJVq38mE3SCtQmzPT5sM5FT84=";
    };
    "eCH-0044-4-0f.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0044-f/4/eCH-0044-4-0f.xsd";
      sha256 = "sha256-c8fy+iuYWdG8f3nUBsw/1hW5Pbn98MBSOfhcyWfbphc=";
    };
    "eCH-0044-4-1.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0044/4/eCH-0044-4-1.xsd";
      sha256 = "sha256-XqYONNLG9Rnkk8sChb7M3aeltjJLcpc1Sz67J1WQCaA=";
    };
    "eCH-0046-5-0f.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0046-f/5/eCH-0046-5-0f.xsd";
      sha256 = "sha256-6zZ0A6jtxbXnc09dLGvdEScKt3HYJubDSvROtOfkQXU=";
    };
    "eCH-0058-5-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0058/5/eCH-0058-5-0.xsd";
      sha256 = "sha256-rJMWhLqYYZX/k4qjNrOudWGWkrrbt6sY7O28REGzB6A=";
    };
    "eCH-0097-2-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0097/2/eCH-0097-2-0.xsd";
      sha256 = "sha256-f7OEp7+0lpkXRs8ImQOP3I1fG4SdahkmLiG8ks4DCRY=";
    };
    "eCH-0097-3-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0097/3/eCH-0097-3-0.xsd";
      sha256 = "sha256-bNgS3NpjwwjwkjFgi+U1wy+HW/EfGEpjxJ8ggbJriBo=";
    };
    "eCH-0097-5-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0097/5/eCH-0097-5-0.xsd";
      sha256 = "sha256-6hqlWmtRN+vK7dEev14zxeZMnHzI1vuTXe0rYXeV/Lo=";
    };
    "eCH-0108-7-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0108/7/eCH-0108-7-0.xsd";
      sha256 = "sha256-zFEPsZde1iJwXf8kf6nOV6l8Rl6mfyyX6fKLMsoz6fE=";
    };
    "eCH-0129-6-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0129/6/eCH-0129-6-0.xsd";
      sha256 = "sha256-i8lSYq0M0atBwK9KtbmMCHAoGPZbSTmY6uB3ihvSzp0=";
    };
    "eCH-0135-1-0.xsd" = {
      url = "https://www.ech.ch/xmlns/eCH-0135/1/eCH-0135-1-0.xsd";
      sha256 = "sha256-a3N+41/HqU+YhhOXm3aiPjzgXx7S8yejEduhgVPpDD8=";
    };
    "eCH-0217-2-0-0.xsd" = {
      url = "https://www.ech.ch/sites/default/files/imce/eCH-Dossier/0211-0240/eCH-0217/2.0.0/Beilagen/eCH-0217-2-0-0.xsd";
      sha256 = "sha256-Ko7LW3lUczEp9zNKisLBjE46u9bzQJLz0hNvVf9Vq/o=";
    };

    # Patched: upstream imports lack schemaLocation attributes, which xmllint
    # needs to resolve them locally. See assets/schemas/README.md.
    "eCH-0119-4-0-0.xsd" = {
      url = "https://share-ech.ch/xmlns/eCH-0119/4.0.0/eCH-0119-4-0-0.xsd";
      sha256 = "sha256-wDUj/cp9u4pzot+KoH3Syh0Z0uai9aeBUtqFUWQctKA=";
      patch = ./schema-patches/eCH-0119-4-0-0.patch;
    };
  };

  # Fetch a schema from upstream, applying its local patch if any.
  schemaFile = name: { url, sha256, patch ? null }:
    let upstream = fetchurl { inherit url; hash = sha256; };
    in
    if patch == null
    then upstream
    else
      runCommandLocal "${name}-patched" { } ''
        cp ${upstream} "$out"
        chmod +w "$out"
        patch -s "$out" < ${patch}
      '';
in
# A directory containing every schema plus the catalog, ready to be embedded.
runCommandLocal "centjes-switzerland-schemas" { } ''
  mkdir -p "$out"
  cp ${catalog} "$out/catalog.xml"
  ${lib.concatStringsSep "\n"
    (lib.mapAttrsToList (name: spec: ''cp ${schemaFile name spec} "$out/${name}"'') schemas)}
''
