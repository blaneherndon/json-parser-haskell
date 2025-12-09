import System.Exit (die)
import System.Environment (getArgs)
import System.IO (readFile)

data JsonValue = JsonString String | JsonNumber Double | JsonObject [(String, JsonValue)] | JsonArray [JsonValue] | JsonTrue | JsonFalse | JsonNull

-- Scanner

main :: IO ()
main = do
    args <- getArgs
    case args of 
        (filePath:_) -> do
    	    contents <- readFile filePath
            putStrLn contents
