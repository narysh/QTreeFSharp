module QuadTree.Benchmarks.Utils

open System.IO
open BenchmarkDotNet.Configs

type MyConfig() =
    inherit ManualConfig()

let DIR_WITH_MATRICES = "../../../../../../../data/"

let readMtxRaw path directed =
    let getCooList (linewords: seq<string array>) =
        linewords
        |> Seq.map (fun x ->
            ((uint64 x.[0]) - 1UL), ((uint64 x.[1]) - 1UL), (if x.Length = 2 then 1.0 else double x.[2]))
        |> Seq.collect (fun (i, j, v) ->
            if not directed then
                [ (i * 1UL<Matrix.rowindex>, j * 1UL<Matrix.colindex>, v)
                  (j * 1UL<Matrix.rowindex>, i * 1UL<Matrix.colindex>, v) ]
            else
                [ (i * 1UL<Matrix.rowindex>, j * 1UL<Matrix.colindex>, v) ])
        |> List.ofSeq

    let lines = File.ReadLines(path)
    let removedComments = lines |> Seq.skipWhile (fun s -> s.[0] = '%')
    let linewords = removedComments |> Seq.map (fun s -> s.Split [| ' ' |])
    let first = Seq.head linewords

    let nrows, ncols, nnz = uint64 first.[0], uint64 first.[1], int first.[2]

    let tl = Seq.tail linewords

    let lst = getCooList tl

    if (directed && nnz <> lst.Length) || ((not directed) && nnz * 2 <> lst.Length) then
        failwithf
            "Incorrect matrix reading. Path: %A expected nnz: %A actual nnz: %A"
            path
            (if directed then nnz else nnz * 2)
            lst.Length

    let coo =
        Matrix.CoordinateList(nrows * 1UL<Matrix.nrows>, ncols * 1UL<Matrix.ncols>, lst)

    let qt = Matrix.fromCoordinateList coo

    (coo, qt)

let readMtx path directed = readMtxRaw path directed |> snd

/// Benchmarks funnel a Result into a mutable result field; keeping that in one
/// place avoids repeating the same match in every benchmark body.
let assignOnOk (result: Result<'a, 'e>) (assign: 'a -> unit) =
    match result with
    | Ok value -> assign value
    | Error _ -> ()

let assignOrFail (what: string) (result: Result<'a, 'e>) (assign: 'a -> unit) =
    match result with
    | Ok value -> assign value
    | Error _ -> failwith what

let op_add (x: double option) (y: double option) =
    match x, y with
    | Some a, Some b -> Some(a + b)
    | Some a, None
    | None, Some a -> Some a
    | None, None -> None

let op_mult (x: double option) (y: double option) =
    match x, y with
    | Some a, Some b -> Some(a * b)
    | _ -> None

let doubleMap (v: double option) = v |> Option.map (fun x -> x * 2.0)

let doubleMapi (i: uint64<Matrix.rowindex>) (j: uint64<Matrix.colindex>) (v: double option) =
    v |> Option.map (fun x -> x + float (uint64 i) + float (uint64 j))

let doubleMap2i (i: uint64<Matrix.rowindex>) (j: uint64<Matrix.colindex>) (x: double option) (y: double option) =
    match x, y with
    | Some a, Some b -> Some(a + b + float (uint64 i))
    | Some a, None -> Some a
    | None, Some b -> Some b
    | None, None -> None

let sumLookups
    limit
    (coords: (uint64<Matrix.rowindex> * uint64<Matrix.colindex>)[])
    (get: _ -> Result<double option, Matrix.Error>)
    =
    let mutable acc = 0.0
    let last = min limit coords.Length - 1

    for k = 0 to last do
        let (i, j) = coords.[k]

        match get (i, j) with
        | Ok(Some v) -> acc <- acc + v
        | _ -> ()

    acc

let updateLookups<'m>
    limit
    (coords: (uint64<Matrix.rowindex> * uint64<Matrix.colindex>)[])
    (values: double[])
    (m: 'm)
    (update: 'm -> uint64<Matrix.rowindex> -> uint64<Matrix.colindex> -> double -> Result<'m, Matrix.Error>)
    =
    let mutable acc = m
    let last = min limit coords.Length - 1

    for k = 0 to last do
        let (i, j) = coords.[k]

        match update acc i j (values.[k] * 2.0) with
        | Ok updated -> acc <- updated
        | _ -> ()

    acc

let sumCells size (get: uint64<Matrix.rowindex> -> uint64<Matrix.colindex> -> Result<double option, Matrix.Error>) =
    let mutable acc = 0.0
    let last = uint64 size - 1UL

    for i in 0UL .. last do
        for j in 0UL .. last do
            match get (i * 1UL<Matrix.rowindex>) (j * 1UL<Matrix.colindex>) with
            | Ok(Some v) -> acc <- acc + v
            | _ -> ()

    acc

let updateCells<'m>
    size
    (m: 'm)
    (update: 'm -> uint64<Matrix.rowindex> -> uint64<Matrix.colindex> -> double -> Result<'m, Matrix.Error>)
    =
    let mutable acc = m
    let last = uint64 size - 1UL

    for i in 0UL .. last do
        for j in 0UL .. last do
            match update acc (i * 1UL<Matrix.rowindex>) (j * 1UL<Matrix.colindex>) 42.0 with
            | Ok updated -> acc <- updated
            | _ -> ()

    acc
