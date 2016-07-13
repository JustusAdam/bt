# Bachelor Thesis Source Repository

This repository contains the LaTeX sources for what will become my bachelor thesis.

## Building

Because I like Haskell and I've always wanted to try the build system `shake` building this requires the following programs be installed:

- A LaTeX distribution including `pdflatex`
- The Haskell tool `stack`

The libraries used are automatically installed by stack if you run `stack install`.
You can then build the thesis by running `stack exec -- bt`.

### Preview

There is a preview of the compiled thesis pdf provided by the GitLab CI server. You can click on the latest (successful) build and use the download artefacts button.

## Timetable

- Begin working period: 31. May 2016
- Status presentation: Mid July 2016
- End working period: 21. August 2016

## Notes

- lint using this https://github.com/btford/write-good
- What does GHC8 do to our generated applicative code?
