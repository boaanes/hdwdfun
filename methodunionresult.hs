[
    (NodeVar "area",[]),
    (NodeVar "height",[NodeMet "m1",NodeMet "m2"]),
    (NodeVar "perimeter",[]),
    (NodeVar "width",[NodeMet "m1",NodeMet "m2"]),
    (NodeMet "m1",[NodeVar "area"]),
    (NodeMet "m2",[NodeVar "perimeter"])
]


[
    [
        (NodeVar "area",[]),
        (NodeVar "height",[NodeMet "m1",NodeMet "m2"]),
        (NodeVar "perimeter",[]),
        (NodeVar "width",[NodeMet "m1",NodeMet "m2"]),
        (NodeMet "m1",[NodeVar "area"]),
        (NodeMet "m2",[NodeVar "perimeter"])
    ],
    [
        (NodeVar "area",[]),
        (NodeVar "height",[NodeMet "m1"]),
        (NodeVar "perimeter",[NodeMet "m4"]),
        (NodeVar "width",[NodeMet "m1",NodeMet "m4"]),
        (NodeMet "m1",[NodeVar "area"]),
        (NodeMet "m4",[NodeVar "height"])
    ],
    [
        (NodeVar "area",[]),
        (NodeVar "height",[NodeMet "m1",NodeMet "m5"]),
        (NodeVar "perimeter",[NodeMet "m5"]),
        (NodeVar "width",[NodeMet "m1"]),
        (NodeMet "m1",[NodeVar "area"]),
        (NodeMet "m5",[NodeVar "width"])
    ],
    [
        (NodeVar "area",[NodeMet "m3"]),
        (NodeVar "height",[NodeMet "m2"]),
        (NodeVar "perimeter",[]),
        (NodeVar "width",[NodeMet "m2"]),
        (NodeMet "m2",[NodeVar "perimeter"]),
        (NodeMet "m3",[NodeVar "height",NodeVar "width"])
    ]
]

-- plan with cyclic stays 
[
    edges
    [
        (NodeVar "height",NodeMet "m1"),
        (NodeVar "height",NodeMet "m2"),
        (NodeVar "width",NodeMet "m1"),
        (NodeVar "width",NodeMet "m2"),
        (NodeMet "m1",NodeVar "area"),
        (NodeMet "m2",NodeVar "perimeter")
    ],
    edges
    [
        (NodeVar "height",NodeMet "m1"),
        (NodeVar "perimeter",NodeMet "m4"),
        (NodeVar "width",NodeMet "m1"),
        (NodeVar "width",NodeMet "m4"),
        (NodeMet "m1",NodeVar "area"),
        (NodeMet "m4",NodeVar "height")
    ],
    edges
    [
        (NodeVar "height",NodeMet "m1"),
        (NodeVar "height",NodeMet "m5"),
        (NodeVar "perimeter",NodeMet "m5"),
        (NodeVar "width",NodeMet "m1"),
        (NodeMet "m1",NodeVar "area"),
        (NodeMet "m5",NodeVar "width")
    ],
    edges
    [
        (NodeVar "area",NodeMet "m3"),
        (NodeVar "height",NodeMet "m2"),
        (NodeVar "width",NodeMet "m2"),
        (NodeMet "m2",NodeVar "perimeter"),
        (NodeMet "m3",NodeVar "height"),
        (NodeMet "m3",NodeVar "width")
    ]
]

-- plan without cyclic stays, seems to result in only stay constraints
[
    edges
    [
        (NodeMet "mArea",NodeVar "area"),
        (NodeMet "mHeight",NodeVar "height"),
        (NodeMet "mPerimeter",NodeVar "perimeter"),
        (NodeMet "mWidth",NodeVar "width")
    ]
]

-- again without cyclic stays, but only called with stayWidth and stayHeight
-- this seems to work
[
    edges
    [
        (NodeVar "height",NodeMet "m1"),
        (NodeVar "height",NodeMet "m2"),
        (NodeVar "width",NodeMet "m1"),
        (NodeVar "width",NodeMet "m2"),
        (NodeMet "m1",NodeVar "area"),
        (NodeMet "m2",NodeVar "perimeter"),
        (NodeMet "mHeight",NodeVar "height"),
        (NodeMet "mWidth",NodeVar "width")
    ]
]

-- result of topSort of above result (filtering out variables)
[
    NodeMet "mHeight",
    NodeMet "mWidth",
    NodeMet "m1",
    NodeMet "m2",
]

--- area as highest and only priority
[
    edges
    [
        (NodeVar "area",NodeMet "m3"),
        (NodeVar "height",NodeMet "m2"),
        (NodeVar "width",NodeMet "m2"),
        (NodeMet "m2",NodeVar "perimeter"),
        (NodeMet "m3",NodeVar "height"),
        (NodeMet "m3",NodeVar "width"),
        (NodeMet "mArea",NodeVar "area")
    ]
]

-- topsort when area has highest and only priority
[
    NodeMet "mArea",
    NodeMet "m3",
    NodeMet "m2",
]

-- area as highest, perimeter as second highest
[
    edges
    [
        (NodeVar "area",NodeMet "m3"),
        (NodeMet "m3",NodeVar "height"),
        (NodeMet "m3",NodeVar "width"),
        (NodeMet "mArea",NodeVar "area"),
        (NodeMet "mPerimeter",NodeVar "perimeter")
    ]
]



[
    edges
    [
        (NodeVar "area",NodeMet "m3"),
        (NodeVar "height",NodeMet "m2"),
        (NodeVar "width",NodeMet "m2"),
        (NodeMet "m2",NodeVar "perimeter"),
        (NodeMet "m3",NodeVar "height"),
        (NodeMet "m3",NodeVar "width"),
        (NodeMet "mArea",NodeVar "area")
    ]
]

-- constraintA <> constraintB
[
    edges
    [
        (NodeVar "height",NodeMet "m1"),
        (NodeVar "height",NodeMet "m2"),
        (NodeVar "width",NodeMet "m1"),
        (NodeVar "width",NodeMet "m2"),
        (NodeMet "m1",NodeVar "area"),
        (NodeMet "m2",NodeVar "perimeter")
    ],
    edges
    [
        (NodeVar "height",NodeMet "m1"),
        (NodeVar "perimeter",NodeMet "m4"),
        (NodeVar "width",NodeMet "m1"),
        (NodeVar "width",NodeMet "m4"),
        (NodeMet "m1",NodeVar "area"),
        (NodeMet "m4",NodeVar "height")
    ],
    edges
    [
        (NodeVar "height",NodeMet "m1"),
        (NodeVar "height",NodeMet "m5"),
        (NodeVar "perimeter",NodeMet "m5"),
        (NodeVar "width",NodeMet "m1"),
        (NodeMet "m1",NodeVar "area"),
        (NodeMet "m5",NodeVar "width")
    ],
    edges
    [
        (NodeVar "area",NodeMet "m3"),
        (NodeVar "height",NodeMet "m2"),
        (NodeVar "width",NodeMet "m2"),
        (NodeMet "m2",NodeVar "perimeter"),
        (NodeMet "m3",NodeVar "height"),
        (NodeMet "m3",NodeVar "width")
    ]
]

-- combinations
[
    [
        Constraint [edge (NodeMet "mArea") (NodeVar "area")],
        Constraint [edge (NodeMet "mPerimeter") (NodeVar "perimeter")],
        Constraint [edge (NodeMet "mWidth") (NodeVar "width")],
        Constraint [edge (NodeMet "mHeight") (NodeVar "height")]
    ],
    [
        Constraint [edge (NodeMet "mArea") (NodeVar "area")],
        Constraint [edge (NodeMet "mPerimeter") (NodeVar "perimeter")],
        Constraint [edge (NodeMet "mWidth") (NodeVar "width")]
    ],
    [
        Constraint [edge (NodeMet "mArea") (NodeVar "area")],
        Constraint [edge (NodeMet "mPerimeter") (NodeVar "perimeter")],
        Constraint [edge (NodeMet "mHeight") (NodeVar "height")]
    ],
    [
        Constraint [edge (NodeMet "mArea") (NodeVar "area")],
        Constraint [edge (NodeMet "mPerimeter") (NodeVar "perimeter")]
    ],
    [
        Constraint [edge (NodeMet "mArea") (NodeVar "area")],
        Constraint [edge (NodeMet "mWidth") (NodeVar "width")],
        Constraint [edge (NodeMet "mHeight") (NodeVar "height")]
    ],
    [
        Constraint [edge (NodeMet "mArea") (NodeVar "area")],
        Constraint [edge (NodeMet "mWidth") (NodeVar "width")]
    ],
    [
        Constraint [edge (NodeMet "mArea") (NodeVar "area")],
        Constraint [edge (NodeMet "mHeight") (NodeVar "height")]
    ],
    [
        Constraint [edge (NodeMet "mArea") (NodeVar "area")]
    ],
    [
        Constraint [edge (NodeMet "mPerimeter") (NodeVar "perimeter")],
        Constraint [edge (NodeMet "mWidth") (NodeVar "width")],
        Constraint [edge (NodeMet "mHeight") (NodeVar "height")]
    ],
    [
        Constraint [edge (NodeMet "mPerimeter") (NodeVar "perimeter")],
        Constraint [edge (NodeMet "mWidth") (NodeVar "width")]
    ],
    [
        Constraint [edge (NodeMet "mPerimeter") (NodeVar "perimeter")],
        Constraint [edge (NodeMet "mHeight") (NodeVar "height")]
    ],
    [
        Constraint [edge (NodeMet "mPerimeter") (NodeVar "perimeter")]
    ],
    [
        Constraint [edge (NodeMet "mWidth") (NodeVar "width")],
        Constraint [edge (NodeMet "mHeight") (NodeVar "height")]
    ],
    [
        Constraint [edge (NodeMet "mWidth") (NodeVar "width")]
    ],
    [
        Constraint [edge (NodeMet "mHeight") (NodeVar "height")]
    ],
    []
]

-- findBestSolution first try with [sA, sP, sW, sH]
[
    (NodeVar "area",NodeMet "m3"),
    (NodeVar "height",NodeMet "m2"),
    (NodeVar "width",NodeMet "m2"),
    (NodeMet "m2",NodeVar "perimeter"),
    (NodeMet "m3",NodeVar "height"),
    (NodeMet "m3",NodeVar "width"),
    (NodeMet "mArea",NodeVar "area")
]

-- [sP, sH, sA, sW]
[
    (NodeVar "height",NodeMet "m1"),
    (NodeVar "height",NodeMet "m5"),
    (NodeVar "perimeter",NodeMet "m5"),
    (NodeVar "width",NodeMet "m1"),
    (NodeMet "m1",NodeVar "area"),
    (NodeMet "m5",NodeVar "width"),
    (NodeMet "mHeight",NodeVar "height"),
    (NodeMet "mPerimeter",NodeVar "perimeter")
]
