load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

PACKAGES = ["array", "base", "text", "containers", "attoparsec", "split", "unordered-containers"]

def aoc_day(day):
    haskell_binary(
        name = "d{}".format(day),
        srcs = ["Day{}.hs".format(day)],
        deps = ["//:lib"] + ["@stackage//{}".format(it) for it in PACKAGES],
        data = native.glob(["{}/*.txt".format(day)]),
    )
