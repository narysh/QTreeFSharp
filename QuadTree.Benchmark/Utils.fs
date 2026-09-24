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
