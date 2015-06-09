Sci --- a scientific computing library in Haskell.
Author: Noah Halford

The Sci library contains the following modules

-- Sci.Calculus
-- Sci.DataProcessing
-- Sci.Matrix
-- Sci.Matrix.Sparse

and depends on the following packages:

array
containers

It is generally recommended to use high-precision numbers from
the Data.Number.Fixed module, included in the numbers package,
whenever possible.

The Sci.Calculus module includes functions for numerical integration
and differentiation. In some cases, integration breaks down when
using high-precision numbers. The exact reason for this is unclear,
but it uses a huge amount of RAM and stalls indefinitely in certain
instances when high precision is used. Mileage may vary.

The Sci.DataProcessing module exports only the function loadFile,
which loads a file (typically a tsv) into a matrix.

Sci.Matrix includes functions for working with matrices, while
Sci.Matrix.Sparse includes functions for working with sparse matrices.

The testing suite depends on hspec. It can be run with cabal test.

To install all necessary packages, first set up a cabal sandbox,
and then run the command

cabal install --only-dependencies --enable-tests

Running cabal build sparse-benchmark will build an executable which
creates two large sparse matrices and multiplies them. The same code
in Python and MATLAB can be found in the test/benchmarks directory.
Similarly, full-benchmark does dense matrix multiplication on the same
two (diagonal) matrices, and analogous benchmarks may be found in
test/benchmarks. The full-benchmark test will output a file ("out.txt")
to disk.

The file src/haddock.tar.gz, when uncompressed, contains HTML files
with haddock documentation for this library.
