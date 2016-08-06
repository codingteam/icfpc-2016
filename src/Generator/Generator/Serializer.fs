module Generator.Serializer

open Generator.Definitions

let serializeNumber = function
    | x, _ when x = 0I -> "0"
    | x, y when y = 1I -> x.ToString ()
    | x, y -> sprintf "%A/%A" x y

let serializePoint (x, y) =
    sprintf "%s,%s" (serializeNumber x) (serializeNumber y)

let serializePolygon polygon =
    seq {
        yield string <| List.length polygon
        yield! Seq.map serializePoint polygon
    }

let serializeSegment (x, y) =
    sprintf "%s %s" (serializePoint x) (serializePoint y)

let private serializePolygons polygons = Seq.map serializePolygon polygons |> Seq.concat
let private serializeSegments polygons = Seq.map serializeSegment polygons

let serialize { Silhouette = silhouette; Skeleton = skeleton } =
    seq {
        yield string silhouette.Length
        yield! serializePolygons silhouette
        yield string skeleton.Length
        yield! serializeSegments skeleton
    }
