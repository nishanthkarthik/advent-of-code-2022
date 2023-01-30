load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

PACKAGES = ["array", "base", "text", "containers", "attoparsec", 
            "split", "parallel", "extra", "MemoTrie"]

COMMON_FLAGS = [        
    "-threaded", 
    "-rtsopts",
]

def aoc_day(day):
    haskell_binary(
        name = "d{}".format(day),
        srcs = ["Day{}.hs".format(day)],
        deps = ["//:lib"] + ["@stackage//{}".format(it) for it in PACKAGES],
        data = native.glob(["{}/*.txt".format(day)]),
        ghcopts = select({
            "//:profiling": ["-prof", "-fprof-auto"] + COMMON_FLAGS,
            "//conditions:default": COMMON_FLAGS
        }),
    )
