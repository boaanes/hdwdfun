module Geometry
    ( c1
    , c2
    , c3
    , c4
    , c5
    , stayc1a
    , stayc1p
    , stayc1r
    , stayc2a
    , stayc2p
    , stayc2r
    , stayra
    , stayrp
    , stayrx
    , stayry
    ) where
import           Algebra.Graph.AdjacencyMap
import           HotDrink


c1 :: Constraint
c1 = Constraint [edges [(VertexVar "c1r",VertexMet "mc1_circle_r2ap"),(VertexMet "mc1_circle_r2ap",VertexVar "c1a"),(VertexMet "mc1_circle_r2ap",VertexVar "c1p")],edges [(VertexVar "c1p",VertexMet "mc1_circle_p2ar"),(VertexMet "mc1_circle_p2ar",VertexVar "c1a"),(VertexMet "mc1_circle_p2ar",VertexVar "c1r")],edges [(VertexVar "c1a",VertexMet "mc1_circle_a2rp"),(VertexMet "mc1_circle_a2rp",VertexVar "c1p"),(VertexMet "mc1_circle_a2rp",VertexVar "c1r")]]

c2 :: Constraint
c2 = Constraint [edges [(VertexVar "c1a",VertexMet "meq1_equality_x2y"),(VertexMet "meq1_equality_x2y",VertexVar "ra")],edges [(VertexVar "ra",VertexMet "meq1_equality_y2x"),(VertexMet "meq1_equality_y2x",VertexVar "c1a")]]

c3 :: Constraint
c3 = Constraint [edges [(VertexVar "rx",VertexMet "mr_rectangle_xy2ap"),(VertexVar "ry",VertexMet "mr_rectangle_xy2ap"),(VertexMet "mr_rectangle_xy2ap",VertexVar "ra"),(VertexMet "mr_rectangle_xy2ap",VertexVar "rp")],edges [(VertexVar "ra",VertexMet "mr_rectangle_xa2py"),(VertexVar "rx",VertexMet "mr_rectangle_xa2py"),(VertexMet "mr_rectangle_xa2py",VertexVar "rp"),(VertexMet "mr_rectangle_xa2py",VertexVar "ry")],edges [(VertexVar "rp",VertexMet "mr_rectangle_xp2ay"),(VertexVar "rx",VertexMet "mr_rectangle_xp2ay"),(VertexMet "mr_rectangle_xp2ay",VertexVar "ra"),(VertexMet "mr_rectangle_xp2ay",VertexVar "ry")],edges [(VertexVar "ra",VertexMet "mr_rectangle_ya2xp"),(VertexVar "ry",VertexMet "mr_rectangle_ya2xp"),(VertexMet "mr_rectangle_ya2xp",VertexVar "rp"),(VertexMet "mr_rectangle_ya2xp",VertexVar "rx")],edges [(VertexVar "rp",VertexMet "mr_rectangle_yp2ax"),(VertexVar "ry",VertexMet "mr_rectangle_yp2ax"),(VertexMet "mr_rectangle_yp2ax",VertexVar "ra"),(VertexMet "mr_rectangle_yp2ax",VertexVar "rx")],edges [(VertexVar "ra",VertexMet "mr_rectangle_ap2xy"),(VertexVar "rp",VertexMet "mr_rectangle_ap2xy"),(VertexMet "mr_rectangle_ap2xy",VertexVar "rx"),(VertexMet "mr_rectangle_ap2xy",VertexVar "ry")]]

c4 :: Constraint
c4 = Constraint [edges [(VertexVar "rp",VertexMet "meq2_equality_x2y"),(VertexMet "meq2_equality_x2y",VertexVar "c2p")],edges [(VertexVar "c2p",VertexMet "meq2_equality_y2x"),(VertexMet "meq2_equality_y2x",VertexVar "rp")]]

c5 :: Constraint
c5 = Constraint [edges [(VertexVar "c2r",VertexMet "mc2_circle_r2ap"),(VertexMet "mc2_circle_r2ap",VertexVar "c2a"),(VertexMet "mc2_circle_r2ap",VertexVar "c2p")],edges [(VertexVar "c2p",VertexMet "mc2_circle_p2ar"),(VertexMet "mc2_circle_p2ar",VertexVar "c2a"),(VertexMet "mc2_circle_p2ar",VertexVar "c2r")],edges [(VertexVar "c2a",VertexMet "mc2_circle_a2rp"),(VertexMet "mc2_circle_a2rp",VertexVar "c2p"),(VertexMet "mc2_circle_a2rp",VertexVar "c2r")]]

stayc1r :: Constraint
stayc1r = Constraint [edges [(VertexMet "stayc1r",VertexVar "c1r")]]

stayc1a :: Constraint
stayc1a = Constraint [edges [(VertexMet "stayc1a",VertexVar "c1a")]]

stayc1p :: Constraint
stayc1p = Constraint [edges [(VertexMet "stayc1p",VertexVar "c1p")]]

stayc2r :: Constraint
stayc2r = Constraint [edges [(VertexMet "stayc2r",VertexVar "c2r")]]

stayc2a :: Constraint
stayc2a = Constraint [edges [(VertexMet "stayc2a",VertexVar "c2a")]]

stayc2p :: Constraint
stayc2p = Constraint [edges [(VertexMet "stayc2p",VertexVar "c2p")]]

stayra :: Constraint
stayra = Constraint [edges [(VertexMet "stayra",VertexVar "ra")]]

stayrp :: Constraint
stayrp = Constraint [edges [(VertexMet "stayrp",VertexVar "rp")]]

stayrx :: Constraint
stayrx = Constraint [edges [(VertexMet "stayrx",VertexVar "rx")]]

stayry :: Constraint
stayry = Constraint [edges [(VertexMet "stayry",VertexVar "ry")]]



