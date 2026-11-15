# mbedTLS (vendored)

TLS and X.509 for `std/net`. Version **4.1.1** (the 4.1 long-term-support line, supported until March 2029), Apache-2.0 (see `LICENSE` and `tf-psa-crypto/LICENSE`).

- Source: https://github.com/Mbed-TLS/mbedtls/releases/download/mbedtls-4.1.1/mbedtls-4.1.1.tar.bz2
- SHA-256 of that tarball: `3359a349e23db3d5536fcee032ae7b2ecbfc08972fab643089b5cbf2a375c98c`

It's copied here instead of fetched through `build.zig.zon` because mbedTLS only publishes `.tar.bz2` releases, which Zig's package fetcher can't read, and its git tags don't contain the generated source files.

Only what the build compiles is kept, unchanged: the `.c` and `.h` files of `include/`, `library/`, and `tf-psa-crypto/` `include/`, `core/`, `dispatch/`, `drivers/builtin/`, `extras/`, `platform/` and `utilities/`, plus the two licenses. The default configuration is used. `build.zig` (`addMbedtls`) lists the source files.

To upgrade: download the new release tarball, check its SHA-256 against the published `-sha256sum.txt`, replace these folders with the same subset, update the file lists in `build.zig` and the version and checksum here.
