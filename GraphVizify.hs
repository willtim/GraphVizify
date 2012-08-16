{-# LANGUAGE PackageImports #-}

-------------------------------------------
-- graphvizify - a markdown extension
-- Tim Williams
-- Version 0.001
-- see graphvizify-example.md for examples
-------------------------------------------

import System.Console.GetOpt
import System.FilePath.Windows (pathSeparator)
import Data.Maybe (fromMaybe)
import "mtl" Control.Monad.State
import Data.List (intersperse,intercalate)
import System
import System.Cmd
import System.Directory
import Text.Pandoc
import Text.Pandoc.Shared (tabFilter)
import qualified Data.Map as M

-- | command-line options
data Flag 
     = Version
     | ImageType String
     | ImageDir String
     | GVOptions String
       deriving Show
                
options :: [OptDescr Flag]
options =
     [ Option ['V','?'] ["version"] (NoArg Version) "show version number"
     , Option ['o']     ["output"]  (ReqArg ImageDir "FILE")  "image directory"
     , Option ['T']     ["imagedir"]  (ReqArg ImageType "TYPE")  "image type"
     , Option ['G']     ["graphviz options"]  (ReqArg GVOptions "\"GraphViz OPTIONS\"")  "GraphViz arguments"
     ]

-- | generates new names, runs dot/neato, transforms Markdown AST  
doGraphViz :: [Flag] -> Block -> StateT Int IO Block
doGraphViz flags (CodeBlock (id, [prog], namevals) contents) 
  | prog == "dot" || prog == "neato" = do
  name <- get
  put (name+1)
  let imgType = extension flags
      gvArgs = "-T" ++ imgType ++ " " ++ gvOptions flags ++ " "
      dir = imageDir flags
      prefix = dir ++ [pathSeparator] ++ show name
      imgName = prefix ++ "." ++ imgType
      dotName = prefix ++ ".dot"
  lift $ createDirectoryIfMissing True dir
  lift $ writeFile dotName contents
  lift . system $ intercalate " " [prog,gvArgs,dotName,">",imgName]
  return $ Para [Image (makeInline $ [extractCaption name namevals]) (imgName, "")]
doGraphViz flags x = return x

makeInline :: [String] -> [Inline]
makeInline ss = intersperse Space (map Str ss)

extension :: [Flag] -> String
extension ((ImageType t):fs) = t 
extension (_:fs) = extension fs
extension [] = "png"

imageDir :: [Flag] -> String
imageDir ((ImageDir d):fs) = d
imageDir (_:fs) = imageDir fs
imageDir [] = "."

gvOptions :: [Flag] -> String
gvOptions ((GVOptions o):fs) = o
gvOptions (_:fs) = gvOptions fs
gvOptions [] = ""

-- | extract a caption of the form ("caption","this is the caption")
extractCaption :: Int -> [(String,String)] -> String
extractCaption name namevals = fromMaybe defaultCaption $ M.lookup "caption" m
  where defaultCaption = "Figure "++show name
        m = M.fromList namevals

-- | call with dot/neato args e.g. -Tpng
main :: IO ()
main = do
  args <- getArgs
  flags <- getFlags args
  getContents >>= transform flags >>= putStrLn . writeDoc
  where transform :: [Flag] -> String -> IO Pandoc
        transform flags doc = fmap fst $ runStateT
                        (bottomUpM (doGraphViz flags) (readDoc doc)) 1

getFlags :: [String] -> IO [Flag]
getFlags args =
   case getOpt Permute options args of
     (o,_,[]) -> return o
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: graphvizify -T<image type> -o<image_dir> [-G<other GraphViz options>]"

readDoc = readMarkdown defaultParserState . tabFilter (stateTabStop defaultParserState)
writeDoc = writeMarkdown defaultWriterOptions
 
