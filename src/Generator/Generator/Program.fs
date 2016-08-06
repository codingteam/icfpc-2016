module Generator.Program

open Generator.Definitions
open Generator.Serializer

let size = 1N
let square =
    [ (0N, 0N)
      (size, 0N)
      (size, size)
      (0N, size) ]
let squareSilhouette =
    [ ((0N, 0N), (size, 0N))
      ((size, 0N), (size, size))
      ((size, size), (0N, size))
      ((0N, size), (0N, 0N)) ]
let initialProblem =
    { Silhouette = [ square ]
      Skeleton = squareSilhouette }

[<EntryPoint>]
let main _ =
    let problem = initialProblem
    printfn "%s" (String.concat "\n" <| serialize problem)
    0
