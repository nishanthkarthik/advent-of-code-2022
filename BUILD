load("@//:defs.bzl", "PACKAGES")

load("@rules_haskell//haskell:defs.bzl", "haskell_repl")

haskell_repl(
    name = "hie-bios",
    collect_data = False,
    deps = ["@stackage//{}".format(it) for it in PACKAGES],
)
