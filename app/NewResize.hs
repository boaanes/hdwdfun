module NewResize
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
import           NewDatastruct

h1 :: Method
h1 = stars [(NodeMet "h1", [NodeVar "rh"]), (NodeVar "ih", [NodeMet "h1"]), (NodeVar "ah", [NodeMet "h1"])]

h2 :: Method
h2 = stars [(NodeMet "h2", [NodeVar "ah"]), (NodeVar "ih", [NodeMet "h2"]), (NodeVar "rh", [NodeMet "h2"])]

r1 :: Method
r1 = stars [(NodeMet "r1", [NodeVar "aw"]), (NodeVar "ah", [NodeMet "r1"]), (NodeVar "r", [NodeMet "r1"])]

r2 :: Method
r2 = stars [(NodeMet "r2", [NodeVar "ah"]), (NodeVar "r", [NodeMet "r2"]), (NodeVar "aw", [NodeMet "r2"])]

r3 :: Method
r3 = stars [(NodeMet "r3", [NodeVar "r"]), (NodeVar "aw", [NodeMet "r3"]), (NodeVar "ah", [NodeMet "r3"])]

w1 :: Method
w1 = stars [(NodeMet "w1", [NodeVar "rw"]), (NodeVar "iw", [NodeMet "w1"]), (NodeVar "aw", [NodeMet "w1"])]

w2 :: Method
w2 = stars [(NodeMet "w2", [NodeVar "aw"]), (NodeVar "iw", [NodeMet "w2"]), (NodeVar "rw", [NodeMet "w2"])]



mih :: Method
mih = stars [(NodeMet "mih", [NodeVar "ih"])]

mrh :: Method
mrh = stars [(NodeMet "mrh", [NodeVar "rh"])]

mah :: Method
mah = stars [(NodeMet "mah", [NodeVar "ah"])]

mr :: Method
mr = stars [(NodeMet "mr", [NodeVar "r"])]

maw :: Method
maw = stars [(NodeMet "maw", [NodeVar "aw"])]

mrw :: Method
mrw = stars [(NodeMet "mrw", [NodeVar "rw"])]

miw :: Method
miw = stars [(NodeMet "miw", [NodeVar "iw"])]

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
