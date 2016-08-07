module Generator.Definitions

type Point = BigRational * BigRational
type Polygon = Point list
type Segment = Point * Point
type Skeleton = Segment list
type Silhouette = Polygon list

type Problem =
    { Silhouette : Silhouette
      Skeleton : Skeleton }
