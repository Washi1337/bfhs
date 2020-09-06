module BfHs.Transpiler 
    ( transpile )
    where 

import BfHs.Language
import Data.List

transpile :: BfProgram -> String
transpile program = header ++ body ++ footer
    where 
        header = unlines [
                "#include <stdio.h>", 
                "#include <stdlib.h>", 
                "", 
                "int main(int argc, char** argv) {",
                indent ++ "char* buffer = (char*) calloc(30000, sizeof(char));",
                indent ++ "int idx = 0;",
                indent ++ "setvbuf(stdout, NULL, _IONBF, 0);",
                ""
            ]

        body   = transpileProgram 1 program

        footer = unlines [
                "",
                indent ++ "free(buffer);",
                "}"
            ]

        indent = (indentation 1) 

indentation :: Int -> String 
indentation level = take (level*4) $ repeat ' '

transpileProgram :: Int -> BfProgram -> String 
transpileProgram level program = intercalate "\n" $ map (transpileNode level) program

transpileNode :: Int -> BfAstNode -> String 
transpileNode level node = case node of 
    MoveLeft -> (indentation level) ++ "idx--;"
    MoveRight -> (indentation level) ++ "idx++;"
    Increment -> (indentation level) ++ "buffer[idx]++;"
    Decrement -> (indentation level) ++ "buffer[idx]--;"
    Output -> (indentation level) ++ "putchar(buffer[idx]);"
    Input -> (indentation level) ++ "buffer[idx] = getchar();"
    Loop subProgram -> intercalate "\n" [header, loopBody, footer]
        where
            header = (indentation level) ++ "while (buffer[idx] != 0) {" 
            loopBody = transpileProgram (level + 1) subProgram
            footer = (indentation level) ++ "}"
