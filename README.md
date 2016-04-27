# Bachelor Thesis Source Repository

This repository contains the LaTeX sources for what will become my bachelor thesis. 

## Building

Because I like Haskell and I've always wanted to try the build system `shake` building this requires the following programs be installed:

- PDFLaTeX
- The Haskell platform (GHC + cabal)
- The shake build system (`cabal install shake`)

This list may expand if more programs become necessary for compiling graphs and images.

Building the pdf is then simply a matter of running `./build.sh`.