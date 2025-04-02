# Notes about XML schemas

There are xsd files on http://ech.ch but they are broken;
* They reference eachother with `http` (not `https`).
* They lack `schemaLocation` attributes sometimes.
* There is no centralised catalog.

Because `xmllint` needs local files; it refuses to download external schemas,
we need to download them ourselves and create a catalog.
The catalog in `catalog.xml` is manually written for validation.

We also had to add some (local) `schemaLocation`s in `eCH-0119-4-0-0.xsd` to get it to compile.
