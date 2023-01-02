load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

PACKAGES = ["base", "text", "containers", "attoparsec"]

def aoc():
    haskell_binary(
        name = "main",
        srcs = ["Main.hs"],
        deps = ["@stackage//{}".format(it) for it in PACKAGES],
        data = native.glob(["*.txt"]),
    )
