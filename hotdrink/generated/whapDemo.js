import { sqrt } from './bo.js';
// create a component and emplace some variables
export const whap = new hd.Component("whap")
const w = whap.emplaceVariable("w", 10)
const h = whap.emplaceVariable("h", 10)
const a = whap.emplaceVariable("a")
const p = whap.emplaceVariable("p")

// create a constraint spec
const area = new hd.Method(3, [0,1], [2], [hd.maskNone], (w,h) => w * h)
const widthAndHeight = new hd.Method(3, [2], [0,1], [hd.maskNone], (a) => [sqrt(a),sqrt(a)])

const awhSpec = new hd.ConstraintSpec([area,widthAndHeight])

// create a constraint spec
const perimiter = new hd.Method(3, [0,1], [2], [hd.maskNone], (w,h) => 2 * (w + h))
const width = new hd.Method(3, [2,1], [0], [hd.maskNone], (p,h) => p / 2 - h)
const height = new hd.Method(3, [2,0], [1], [hd.maskNone], (p,w) => p / 2 - w)

const pwhSpec = new hd.ConstraintSpec([perimiter,width,height])

// emplace a constraint built from the constraint spec
const awh = whap.emplaceConstraint("awh", awhSpec, [w,h,a])

// emplace a constraint built from the constraint spec
const pwh = whap.emplaceConstraint("pwh", pwhSpec, [w,h,p])

