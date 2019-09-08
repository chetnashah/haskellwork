import System.IO
import System.Directory
import Data.Text

getFileContents :: String -> IO String
getFileContents fileName = 
  case fileName of
    -- ignoring directories
    "." -> return ("")
    ".." -> return ("..")
    ".ghc" -> return (".ghc")
    -- for regular files
    _ -> do
      fH <- openFile fileName ReadMode
      contents <- hGetContents fH
      let copycontent = contents
      return copycontent

main = do
        putStrLn "Greetings! what is your name?"
        inpStr <- getLine
        putStrLn $ "Welcome to haskell, " ++ inpStr ++ "!"
        
        dirs <- getDirectoryContents "."
        mapM_ putStrLn dirs
        let allContentIO = Prelude.map getFileContents dirs
        let allContent = sequence allContentIO
        allContentString <- allContent
        mapM_ putStrLn allContentString
