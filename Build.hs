import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

buildDir = "_build"
sourceDir = "src"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do
    want ["thesis.pdf"]

    "*.pdf" %> \out -> do
        let srcRel =  out -<.> "tex"
        let src = sourceDir </> srcRel
        need [src]
        [_, Exit e] <- replicateM 2 $ command [Cwd sourceDir] "pdflatex" ["-interaction=nonstopmode", srcRel]
        -- trackWrite [buildDir </> out]
        copyFileChanged (sourceDir </> out) out
