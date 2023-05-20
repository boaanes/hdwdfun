module TestData
    ( testVars
    , testCons
    , testOrder
    )
    where

import           AST
import           Data.Map
import           HotDrinkF

testVars :: Map String (Maybe Value)
testVars = fromList [("w", Just (DoubleVal 10)), ("h", Just (DoubleVal 10)), ("a", Just (DoubleVal 100)), ("p", Just (DoubleVal 40))]

testCons :: [Constraint]
testCons =
    [ Constraint
        [ methodToGraph ["w", "h"] ("m1", [("a", BinOp "*" (Var "w") (Var "h"))])
        , methodToGraph ["a"] ("m2", [("w", UnOp "sqrt" (Var "a")), ("h", UnOp "sqrt" (Var "a"))])
        ]
    , Constraint
        [ methodToGraph ["w", "h"] ("m3", [("p", BinOp "*" (Lit (DoubleVal 2)) (BinOp "+" (Var "w")  (Var "h")))])
        , methodToGraph ["w", "p"] ("m4", [("h", BinOp "-" (BinOp "/" (Var "p") (Lit (DoubleVal 2))) (Var "w"))])
        , methodToGraph ["h", "p"] ("m5", [("w", BinOp "-" (BinOp "/" (Var "p") (Lit (DoubleVal 2))) (Var "h"))])
        ]
    ]

testOrder :: [String]
testOrder = ["a", "p", "w", "h"]
