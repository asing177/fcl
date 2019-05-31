workspace(name = "FCL")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)

# Download `rules_haskell`.
# and make it accessible `@io_tweag_rules_haskell`.
http_archive(
    name = "io_tweag_rules_haskell",
    strip_prefix = "rules_haskell-0.9",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.9.tar.gz"],
    sha256 = "a4399554303e4de85d97c2deede5b173e0f7d2485a7291ee932eff693912c2ab",
)

load(
    "@io_tweag_rules_haskell//haskell:repositories.bzl",
    "haskell_repositories"
)

# `haskell_repositories()` sets up all bazel dependencies
# required by `rules_haskell`.
haskell_repositories()

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_register_ghc_bindists",
)

# Registers a haskell toolchain with a GHC binary
# downloaded from haskell.org.
haskell_register_ghc_bindists(version = "8.6.4")
