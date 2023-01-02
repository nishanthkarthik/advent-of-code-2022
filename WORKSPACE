load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "rules_haskell",
    sha256 = "2a07b55c30e526c07138c717b0343a07649e27008a873f2508ffab3074f3d4f3",
    strip_prefix = "rules_haskell-0.16",
    url = "https://github.com/tweag/rules_haskell/archive/refs/tags/v0.16.tar.gz",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")

# https://www.stackage.org/lts-20.5
rules_haskell_toolchains(version = "9.2.5")

# Stackage

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

load("@//:defs.bzl", "PACKAGES")

stack_snapshot(
    name = "stackage",
    packages = PACKAGES,
    snapshot = "lts-20.5",
    stack_snapshot_json = "//:stackage_snapshot.json",
    components = { "attoparsec": ["lib:attoparsec", "lib:attoparsec-internal"] },
    components_dependencies = {
        # https://github.com/tweag/rules_haskell/issues/1439#issuecomment-1022055315
        "attoparsec": json.encode({"lib:attoparsec": ["lib:attoparsec-internal"]}),
    },
)
