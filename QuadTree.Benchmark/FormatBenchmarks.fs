namespace QuadTree.Benchmarks.Formats

open OptionMonoid
open System
open BenchmarkDotNet.Attributes
open Matrix
open COOArray
open QuadTree.Benchmarks.Utils

[<Config(typeof<QuadTree.Benchmarks.Utils.MyConfig>)>]
type FormatBenchmark() =

    let mutable cooMatrix1 = Unchecked.defaultof<ArrayCOO<double>>
    let mutable cooMatrix2 = Unchecked.defaultof<ArrayCOO<double>>
    let mutable qtMatrix1 = Unchecked.defaultof<SparseMatrix<double>>
    let mutable qtMatrix2 = Unchecked.defaultof<SparseMatrix<double>>
    let mutable listMatrix1 = Unchecked.defaultof<COOList.ListCOO<double>>
    let mutable listMatrix2 = Unchecked.defaultof<COOList.ListCOO<double>>

    let mutable lookupCoords: (uint64<rowindex> * uint64<colindex>) array = [||]
    let mutable lookupValues: double array = [||]

    let mutable resultCoo = Unchecked.defaultof<ArrayCOO<double>>
    let mutable resultQt = Unchecked.defaultof<SparseMatrix<double>>
    let mutable resultList = Unchecked.defaultof<COOList.ListCOO<double>>
    let mutable resultCooVal = 0.0
    let mutable resultQtVal = 0.0
    let mutable resultListVal = 0.0

    [<Params(256, 512, 1024)>]
    member val Size = 0 with get, set

    [<Params(0.01, 0.05)>]
    member val FillRate = 0.0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let rng = Random(42)
        let size = uint64 this.Size
        let totalCells = float (size * size)
        let targetNnz = max 10 (int (totalCells * this.FillRate))

        let generateEntries count =
            if count >= int (size * size) then
                [ for i in 0UL .. size - 1UL do
                      for j in 0UL .. size - 1UL do
                          (i * 1UL<rowindex>, j * 1UL<colindex>, rng.NextDouble() * 100.0) ]
            else
                let entries = System.Collections.Generic.HashSet<uint64 * uint64>()

                [ 1..count ]
                |> List.map (fun _ ->
                    let mutable i = 0UL
                    let mutable j = 0UL

                    while entries.Contains((i, j)) || i >= size || j >= size do
                        i <- uint64 (rng.Next(int size))
                        j <- uint64 (rng.Next(int size))

                    entries.Add((i, j)) |> ignore
                    (i * 1UL<rowindex>, j * 1UL<colindex>, rng.NextDouble() * 100.0))
                |> List.sort

        let entries1 = generateEntries targetNnz
        let entries2 = generateEntries targetNnz

        let coo1 = CoordinateList(size * 1UL<nrows>, size * 1UL<ncols>, entries1)
        let coo2 = CoordinateList(size * 1UL<nrows>, size * 1UL<ncols>, entries2)
        cooMatrix1 <- new ArrayCOO<double>(size * 1UL<nrows>, size * 1UL<ncols>, entries1)
        cooMatrix2 <- new ArrayCOO<double>(size * 1UL<nrows>, size * 1UL<ncols>, entries2)

        qtMatrix1 <-
            match fromCoordinateList coo1 with
            | Ok m -> m
            | Error e -> failwithf "fromCoordinateList: %s" e

        qtMatrix2 <-
            match fromCoordinateList coo2 with
            | Ok m -> m
            | Error e -> failwithf "fromCoordinateList: %s" e

        listMatrix1 <- COOList.fromArray cooMatrix1
        listMatrix2 <- COOList.fromArray cooMatrix2

        lookupCoords <- entries1 |> List.map (fun (i, j, _) -> (i, j)) |> Array.ofList
        lookupValues <- entries1 |> List.map (fun (_, _, v) -> v) |> Array.ofList

    [<Benchmark(Baseline = true, Description = "COO_map")>]
    member this.CooMap() =
        resultCoo <- cooMap cooMatrix1 (fun v -> v |> Option.map (fun x -> x * 2.0))

    [<Benchmark(Description = "QT_map")>]
    member this.QtMap() =
        resultQt <- map qtMatrix1 (fun v -> v |> Option.map (fun x -> x * 2.0))

    [<Benchmark(Description = "COO_mapi")>]
    member this.CooMapi() =
        resultCoo <-
            cooMapi cooMatrix1 (fun i j v -> v |> Option.map (fun x -> x + float (uint64 i) + float (uint64 j)))

    [<Benchmark(Description = "QT_mapi")>]
    member this.QtMapi() =
        resultQt <- mapi qtMatrix1 (fun i j v -> v |> Option.map (fun x -> x + float (uint64 i) + float (uint64 j)))

    [<Benchmark(Description = "COO_map2")>]
    member this.CooMap2() =
        match cooMap2 cooMatrix1 cooMatrix2 op_add with
        | Ok r -> resultCoo <- r
        | Error _ -> ()

    [<Benchmark(Description = "QT_map2")>]
    member this.QtMap2() =
        match map2 qtMatrix1 qtMatrix2 op_add with
        | Ok r -> resultQt <- r
        | Error _ -> ()

    [<Benchmark(Description = "COO_map2i")>]
    member this.CooMap2i() =
        match cooMap2i cooMatrix1 cooMatrix2 map2iLogic with
        | Ok r -> resultCoo <- r
        | Error _ -> ()

    [<Benchmark(Description = "QT_map2i")>]
    member this.QtMap2i() =
        match map2i qtMatrix1 qtMatrix2 map2iLogic with
        | Ok r -> resultQt <- r
        | Error _ -> ()

    [<Benchmark(Description = "COOLIST_map")>]
    member this.CooListMap() =
        resultList <- COOList.cooMap listMatrix1 (fun v -> v |> Option.map (fun x -> x * 2.0))

    [<Benchmark(Description = "COOLIST_mapi")>]
    member this.CooListMapi() =
        resultList <-
            COOList.cooMapi listMatrix1 (fun i j v ->
                v |> Option.map (fun x -> x + float (uint64 i) + float (uint64 j)))

    [<Benchmark(Description = "COOLIST_map2")>]
    member this.CooListMap2() =
        match COOList.cooMap2 listMatrix1 listMatrix2 op_add with
        | Ok r -> resultList <- r
        | Error _ -> ()

    [<Benchmark(Description = "COOLIST_map2i")>]
    member this.CooListMap2i() =
        match COOList.cooMap2i listMatrix1 listMatrix2 map2iLogic with
        | Ok r -> resultList <- r
        | Error _ -> ()

    [<Benchmark(Description = "COOLIST_mxm")>]
    member this.CooListMxm() =
        match COOList.mxmcoo op_add op_mult listMatrix1 listMatrix1 with
        | Ok result -> resultList <- result
        | Error _ -> failwith "COOList mxmcoo failed"

    [<Benchmark(Description = "COOLIST_get")>]
    member this.CooListGet() =
        let n = min lookupCoords.Length 1000

        resultListVal <-
            QuadTree.Benchmarks.Utils.sumLookups n lookupCoords (fun i j -> COOList.cooGet (listMatrix1, i, j))

    [<Benchmark(Description = "COOLIST_set")>]
    member this.CooListSet() =
        let n = min lookupCoords.Length 1000

        resultList <-
            QuadTree.Benchmarks.Utils.updateLookups n lookupCoords lookupValues listMatrix1 (fun m i j v ->
                COOList.cooUpdate (m, i, j, v))

    [<Benchmark(Description = "COO_get")>]
    member this.CooGet() =
        let n = min lookupCoords.Length 1000
        resultCooVal <- QuadTree.Benchmarks.Utils.sumLookups n lookupCoords (fun i j -> cooGet (cooMatrix1, i, j))

    [<Benchmark(Description = "QT_get")>]
    member this.QtGet() =
        let n = min lookupCoords.Length 1000
        resultQtVal <- QuadTree.Benchmarks.Utils.sumLookups n lookupCoords (fun i j -> get qtMatrix1 i j)

    [<Benchmark(Description = "COO_set")>]
    member this.CooSet() =
        let n = min lookupCoords.Length 1000

        resultCoo <-
            QuadTree.Benchmarks.Utils.updateLookups n lookupCoords lookupValues cooMatrix1 (fun m i j v ->
                cooUpdate (m, i, j, v))

    [<Benchmark(Description = "QT_set")>]
    member this.QtSet() =
        let n = min lookupCoords.Length 1000

        resultQt <-
            QuadTree.Benchmarks.Utils.updateLookups n lookupCoords lookupValues qtMatrix1 (fun m i j v -> set m i j v)

    [<Benchmark(Description = "COO_mxm")>]
    member this.CooMxm() =
        match mxmcoo op_add op_mult cooMatrix1 cooMatrix1 with
        | Ok result -> resultCoo <- result
        | Error _ -> failwith "mxmcoo failed"

    [<Benchmark(Description = "QT_mxm")>]
    member this.QtMxm() =
        match LinearAlgebra.mxm op_add op_mult qtMatrix1 qtMatrix1 with
        | Ok result -> resultQt <- result
        | Error _ -> failwith "mxm failed"


[<Config(typeof<QuadTree.Benchmarks.Utils.MyConfig>)>]
type DenseFormatBenchmark() =

    let mutable cooMatrix = Unchecked.defaultof<ArrayCOO<double>>
    let mutable qtMatrix = Unchecked.defaultof<SparseMatrix<double>>
    let mutable listMatrix = Unchecked.defaultof<COOList.ListCOO<double>>

    let mutable resultCoo = Unchecked.defaultof<ArrayCOO<double>>
    let mutable resultQt = Unchecked.defaultof<SparseMatrix<double>>
    let mutable resultList = Unchecked.defaultof<COOList.ListCOO<double>>
    let mutable resultCooVal = 0.0
    let mutable resultQtVal = 0.0
    let mutable resultListVal = 0.0

    [<Params(64, 128, 256)>]
    member val Size = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let rng = Random(42)
        let size = uint64 this.Size

        let entries =
            [ for i in 0UL .. size - 1UL do
                  for j in 0UL .. size - 1UL do
                      (i * 1UL<rowindex>, j * 1UL<colindex>, rng.NextDouble() * 100.0) ]

        let coo = CoordinateList(size * 1UL<nrows>, size * 1UL<ncols>, entries)
        cooMatrix <- new ArrayCOO<double>(size * 1UL<nrows>, size * 1UL<ncols>, entries)

        qtMatrix <-
            match fromCoordinateList coo with
            | Ok m -> m
            | Error e -> failwithf "fromCoordinateList: %s" e

        listMatrix <- COOList.fromArray cooMatrix

    [<Benchmark(Baseline = true, Description = "Dense_COO_map")>]
    member this.DenseCooMap() =
        resultCoo <- cooMap cooMatrix (fun v -> v |> Option.map (fun x -> x * 2.0))

    [<Benchmark(Description = "Dense_QT_map")>]
    member this.DenseQtMap() =
        resultQt <- map qtMatrix (fun v -> v |> Option.map (fun x -> x * 2.0))

    [<Benchmark(Description = "Dense_COO_mapi")>]
    member this.DenseCooMapi() =
        resultCoo <- cooMapi cooMatrix (fun i j v -> v |> Option.map (fun x -> x + float (uint64 i) + float (uint64 j)))

    [<Benchmark(Description = "Dense_QT_mapi")>]
    member this.DenseQtMapi() =
        resultQt <- mapi qtMatrix (fun i j v -> v |> Option.map (fun x -> x + float (uint64 i) + float (uint64 j)))

    [<Benchmark(Description = "Dense_COOLIST_map")>]
    member this.DenseCooListMap() =
        resultList <- COOList.cooMap listMatrix (fun v -> v |> Option.map (fun x -> x * 2.0))

    [<Benchmark(Description = "Dense_COOLIST_mapi")>]
    member this.DenseCooListMapi() =
        resultList <-
            COOList.cooMapi listMatrix (fun i j v -> v |> Option.map (fun x -> x + float (uint64 i) + float (uint64 j)))

    [<Benchmark(Description = "Dense_COOLIST_mxm")>]
    member this.DenseCooListMxm() =
        match COOList.mxmcoo op_add op_mult listMatrix listMatrix with
        | Ok result -> resultList <- result
        | Error _ -> failwith "COOList mxmcoo failed"

    [<Benchmark(Description = "Dense_COOLIST_get")>]
    member this.DenseCooListGet() =
        let mutable acc = 0.0

        for i in 0UL .. uint64 this.Size - 1UL do
            for j in 0UL .. uint64 this.Size - 1UL do
                match COOList.cooGet (listMatrix, i * 1UL<rowindex>, j * 1UL<colindex>) with
                | Ok(Some v) -> acc <- acc + v
                | _ -> ()

        resultListVal <- acc

    [<Benchmark(Description = "Dense_COOLIST_set")>]
    member this.DenseCooListSet() =
        let mutable m = listMatrix
        let size = uint64 this.Size

        for i in 0UL .. size - 1UL do
            for j in 0UL .. size - 1UL do
                match COOList.cooUpdate (m, i * 1UL<rowindex>, j * 1UL<colindex>, 42.0) with
                | Ok updated -> m <- updated
                | _ -> ()

        resultList <- m

    [<Benchmark(Description = "Dense_COO_get")>]
    member this.DenseCooGet() =
        let mutable acc = 0.0

        for i in 0UL .. uint64 this.Size - 1UL do
            for j in 0UL .. uint64 this.Size - 1UL do
                match cooGet (cooMatrix, i * 1UL<rowindex>, j * 1UL<colindex>) with
                | Ok(Some v) -> acc <- acc + v
                | _ -> ()

        resultCoo <- cooMatrix

    [<Benchmark(Description = "Dense_QT_get")>]
    member this.DenseQtGet() =
        let mutable acc = 0.0

        for i in 0UL .. uint64 this.Size - 1UL do
            for j in 0UL .. uint64 this.Size - 1UL do
                match get qtMatrix (i * 1UL<rowindex>) (j * 1UL<colindex>) with
                | Ok(Some v) -> acc <- acc + v
                | _ -> ()

        resultQt <- qtMatrix

    [<Benchmark(Description = "Dense_COO_set")>]
    member this.DenseCooSet() =
        let mutable m = cooMatrix
        let size = uint64 this.Size

        for i in 0UL .. size - 1UL do
            for j in 0UL .. size - 1UL do
                match cooUpdate (m, i * 1UL<rowindex>, j * 1UL<colindex>, 42.0) with
                | Ok updated -> m <- updated
                | _ -> ()

        resultCoo <- m

    [<Benchmark(Description = "Dense_QT_set")>]
    member this.DenseQtSet() =
        let mutable m = qtMatrix
        let size = uint64 this.Size

        for i in 0UL .. size - 1UL do
            for j in 0UL .. size - 1UL do
                match set m (i * 1UL<rowindex>) (j * 1UL<colindex>) 42.0 with
                | Ok updated -> m <- updated
                | _ -> ()

        resultQt <- m

    [<Benchmark(Description = "Dense_COO_mxm")>]
    member this.DenseCooMxm() =
        match mxmcoo op_add op_mult cooMatrix cooMatrix with
        | Ok result -> resultCoo <- result
        | Error _ -> failwith "mxmcoo failed"

    [<Benchmark(Description = "Dense_QT_mxm")>]
    member this.DenseQtMxm() =
        match LinearAlgebra.mxm op_add op_mult qtMatrix qtMatrix with
        | Ok result -> resultQt <- result
        | Error _ -> failwith "mxm failed"
