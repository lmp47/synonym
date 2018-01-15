import System.IO
import System.Environment

gen :: Int -> IO ()
gen n =
    let className = "IFUnsafe" ++ (show n) in
    writeFile (className ++ ".java")
        ("public class " ++ className ++ " {\n" ++
         "    int length;\n" ++
         concat (map (\x -> "    char get" ++ (show x) ++ "(int index);\n") [1..n]) ++
         "    public int equals (" ++ className ++ " o1, " ++
                                      className ++ " o2, " ++
                                      className ++ " priv) {\n" ++
         "        assume (o1.length == o2.length);\n" ++
         "        assume (o1.length == priv.length);\n" ++
         "        int i = 0;\n" ++
         "        while (i < o1.length) {\n" ++
         (concat $ (map genIf1) [1..n]) ++
         "            i++;\n" ++
         "        }\n" ++
         "        i = 0;\n" ++
         "        while (i < o1.length) {\n" ++
         (concat $ (map genIf2) [1..n]) ++
         "            i++;\n" ++
         "        }\n" ++
         "        return 1;\n" ++
         "    }\n" ++
         "}")
  where
    genIf1 n =
      let n' = show n in
      "            if (o1.get" ++ n' ++ "(i) != o2.get" ++ n' ++ "(i)) {\n" ++
      "                return 0;\n" ++
      "            }\n"
    genIf2 n =
      let n' = show n in
      "            if (o1.get" ++ n' ++ "(i) == priv.get" ++ n' ++ "(i)) {\n" ++
      "                return 0;\n" ++
      "            }\n"

main :: IO ()
main = do
  args <- getArgs
  if (args == [])
  then error "Must supply argument"
  else gen (read $ head args)
