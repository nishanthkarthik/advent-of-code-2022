load("@//:defs.bzl", "PACKAGES", "aoc_day")

load("@rules_haskell//haskell:defs.bzl", "haskell_repl", "haskell_library")

haskell_repl(
    name = "hie-bios",
    collect_data = False,
    deps = [":lib"],
)

haskell_library(
    name = "lib",
    srcs = ["Lib.hs"],
    deps = ["@stackage//{}".format(it) for it in PACKAGES],
)

[aoc_day(it + 1) for it in range(4)]
