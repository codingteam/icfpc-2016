module Generator.Geometry

open Generator.Definitions

let private flipPoint ((sx1, sy1), (sx2, sy2)) (x, y) : Point =
    if sx1.Equals(sx2) then
        (2N * sx1 - x, y)
    else
        if sy1.Equals(sy2) then
            (x, 2N*sy1 - y)
        else
            let k = (sy2 - sy1) / (sx2 - sx1)
            let k' = -(sx2-sx1) / (sy2-sy1)
            let b = sy1 - k * sx1
            let b' = y - k' * x
            let x0 = - (b - b') / (k - k')
            let y0 = (b'*k - b*k') / (k - k')
            let x' = 2N*x0 - x
            let y' = 2N*y0 - y
            (x', y')

let private flipPolygon edge polygon : Polygon = List.map (flipPoint edge) polygon
let private flipSilhouette edge silhouette = List.map (flipPolygon edge) silhouette
let private flipSegment edge (p1, p2) = (flipPoint edge p1), (flipPoint edge p2)
let private flipSkeleton edge segments = List.map (flipSegment edge) segments

let flip (edge : Segment) { Silhouette = silhouette; Skeleton = skeleton } : Problem =
    { Silhouette = flipSilhouette edge silhouette
      Skeleton = flipSkeleton edge skeleton }
