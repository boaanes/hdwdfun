module Resize
    ( h1
    , h2
    , mah
    , maw
    , mih
    , miw
    , mr
    , mrh
    , mrw
    , r1
    , r2
    , r3
    , stayAh
    , stayAw
    , stayIh
    , stayIw
    , stayR
    , stayRh
    , stayRw
    , w1
    , w2
    ) where
import           Algebra.Graph.AdjacencyMap
import           HotDrink

h1 :: Method
h1 = stars [(VertexMet "h1", [VertexVar "rh"]), (VertexVar "ih", [VertexMet "h1"]), (VertexVar "ah", [VertexMet "h1"])]

h2 :: Method
h2 = stars [(VertexMet "h2", [VertexVar "ah"]), (VertexVar "ih", [VertexMet "h2"]), (VertexVar "rh", [VertexMet "h2"])]

r1 :: Method
r1 = stars [(VertexMet "r1", [VertexVar "aw"]), (VertexVar "ah", [VertexMet "r1"]), (VertexVar "r", [VertexMet "r1"])]

r2 :: Method
r2 = stars [(VertexMet "r2", [VertexVar "ah"]), (VertexVar "r", [VertexMet "r2"]), (VertexVar "aw", [VertexMet "r2"])]

r3 :: Method
r3 = stars [(VertexMet "r3", [VertexVar "r"]), (VertexVar "aw", [VertexMet "r3"]), (VertexVar "ah", [VertexMet "r3"])]

w1 :: Method
w1 = stars [(VertexMet "w1", [VertexVar "rw"]), (VertexVar "iw", [VertexMet "w1"]), (VertexVar "aw", [VertexMet "w1"])]

w2 :: Method
w2 = stars [(VertexMet "w2", [VertexVar "aw"]), (VertexVar "iw", [VertexMet "w2"]), (VertexVar "rw", [VertexMet "w2"])]



mih :: Method
mih = stars [(VertexMet "mih", [VertexVar "ih"])]

mrh :: Method
mrh = stars [(VertexMet "mrh", [VertexVar "rh"])]

mah :: Method
mah = stars [(VertexMet "mah", [VertexVar "ah"])]

mr :: Method
mr = stars [(VertexMet "mr", [VertexVar "r"])]

maw :: Method
maw = stars [(VertexMet "maw", [VertexVar "aw"])]

mrw :: Method
mrw = stars [(VertexMet "mrw", [VertexVar "rw"])]

miw :: Method
miw = stars [(VertexMet "miw", [VertexVar "iw"])]

stayIh :: Constraint
stayIh = Constraint [mih]

stayRh :: Constraint
stayRh = Constraint [mrh]

stayAh :: Constraint
stayAh = Constraint [mah]

stayR :: Constraint
stayR = Constraint [mr]

stayAw :: Constraint
stayAw = Constraint [maw]

stayRw :: Constraint
stayRw = Constraint [mrw]

stayIw :: Constraint
stayIw = Constraint [miw]
