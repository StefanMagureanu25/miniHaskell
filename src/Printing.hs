
module Printing (showExp) where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar v = getVar v

inParens :: String -> String
inParens s = "(" ++ s ++ ")"

showExp :: ComplexExp -> String
showExp (CX v) = showVar v
showExp (Nat n) = show n
showExp (CLam x e) = inParens ("\\" ++ showVar x ++ " -> " ++ showExp e)
showExp (CApp e1 e2) = inParens (showExp e1 ++ " " ++ showExp e2)
showExp (Let x ex e) = inParens ("let " ++ showVar x ++ " := " ++ showExp ex ++ " in " ++ showExp e)
showExp (LetRec x ex e) = inParens ("letrec " ++ showVar x ++ " := " ++ showExp ex ++ " in " ++ showExp e)
showExp (List l) = "[" ++ intercalate "," (map showExp l) ++ "]"