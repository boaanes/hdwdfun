module Temperature where

{-
c :: Variable
c = ("c", Just (-40))

f :: Variable
f = ("f", Just (-40))

m1 :: Method
m1 = ("m1", [("c", BinOp "*" (BinOp "-" (Var "f") (Lit 32)) (BinOp "/" (Lit 5) (Lit 9)))])

m2 :: Method
m2 = ("m2", [("f", BinOp "+" (BinOp "*" (Var "c") (BinOp "/" (Lit 9) (Lit 5))) (Lit 32))])

f2 :: Variable
f2 = ("f", Just 100)

constraintA :: Constraint
constraintA = (([c, f], [m1, m1]), 1, False)

constraints :: [Constraint]
constraints = [constraintA]

exampleAdjList :: [(VertexType, [VertexType])]
exampleAdjList = [(VertexVar c, [VertexMet m2]), (VertexVar f, [VertexMet m1]), (VertexMet m1, [VertexVar c]), (VertexMet m2, [VertexVar f])]

exampleAdjList2 :: [(VertexType, [VertexType])]
exampleAdjList2 = [(VertexVar c, [VertexMet m2]), (VertexVar f2, [VertexMet m1]), (VertexMet m1, [VertexVar c]), (VertexMet m2, [VertexVar f2])]

exampleGraph :: Graph VertexType
exampleGraph = stars exampleAdjList

exampleGraph2 :: Graph VertexType
exampleGraph2 = stars exampleAdjList2
-}
