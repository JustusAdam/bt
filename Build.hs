import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad (replicateM)

buildDir = "_build"
sourceDir = "src"


buildPFD :: FilePath -> Action ()
buildPFD out = do
    need [src]
    [_, Exit e] <- replicateM 2 $ command [Cwd sourceDir] "pdflatex" ["-interaction=nonstopmode", srcRel]
    -- trackWrite [buildDir </> out]
    copyFileChanged (sourceDir </> out) out
  where
      srcRel =  out -<.> "tex"
      src = sourceDir </> srcRel


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do
    want ["thesis.pdf"]

    "*.pdf" %> buildPFD
