module Generator.Program

open Generator.Definitions
open Generator.Serializer

let size = 1I % 1I
let zero = 0I % 1I
let square =
    [ (zero, zero)
      (size, zero)
      (size, size)
      (zero, size) ]
let squareSilhouette =
    [ ((zero, zero), (size, zero))
      ((size, zero), (size, size))
      ((size, size), (zero, size))
      ((zero, size), (zero, zero)) ]
let initialProblem =
    { Silhouette = [ square ]
      Skeleton = squareSilhouette }

[<EntryPoint>]
let main _ =
    let problem = initialProblem
    printfn "%s" (String.concat "\n" <| serialize problem)
    0
