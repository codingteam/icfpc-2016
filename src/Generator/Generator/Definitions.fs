module Generator.Definitions

type Number = bigint * bigint
let (%) x y : Number = x, y

type Point = Number * Number
type Polygon = Point list
type Segment = Point * Point
type Skeleton = Segment list
type Silhouette = Polygon list

type Problem =
    { Silhouette : Silhouette
      Skeleton : Skeleton }
