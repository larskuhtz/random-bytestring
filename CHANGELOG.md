0.1.3 (2018-03-17)
==================

*   Support for GHC-8.4.1

0.1.2
=====

*   Add support for PCG as PRNG in addition to MWC.

    The module `Data.ByteString.Random.PCG` uses PCG. The module
    `Data.ByteString.Random.MWC` uses MWC. The module `Data.ByteString.Random`
    uses MWC for backward compatibility.

*   Support usage of custom PRNGs through the type class `RandomWords` and the
    function `generate` in the module `Data.ByteString.Random.Internal`.

*   Add some benchmark results to package description

0.1.1
=====

*   Support for GHC-8.2.1 and Cabal-2.0

0.1.0
=====

*   In the implementation of `random`, use `withSystemGen` that seeds the PRNG
    from the system randomness source.

0.0.1
=====

*   First public release
