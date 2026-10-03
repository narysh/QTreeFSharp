namespace QuadTree.Benchmarks.Formats

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
        resultCoo <- cooMap cooMatrix1 doubleMap

    [<Benchmark(Description = "QT_map")>]
    member this.QtMap() = resultQt <- map qtMatrix1 doubleMap

    [<Benchmark(Description = "COO_mapi")>]
    member this.CooMapi() =
        resultCoo <- cooMapi cooMatrix1 doubleMapi

    [<Benchmark(Description = "QT_mapi")>]
    member this.QtMapi() = resultQt <- mapi qtMatrix1 doubleMapi

    [<Benchmark(Description = "COO_map2")>]
    member this.CooMap2() =
        assignOnOk (cooMap2 cooMatrix1 cooMatrix2 op_add) (fun r -> resultCoo <- r)

    [<Benchmark(Description = "QT_map2")>]
    member this.QtMap2() =
        assignOnOk (map2 qtMatrix1 qtMatrix2 op_add) (fun r -> resultQt <- r)

    [<Benchmark(Description = "COO_map2i")>]
    member this.CooMap2i() =
        assignOnOk (cooMap2i cooMatrix1 cooMatrix2 doubleMap2i) (fun r -> resultCoo <- r)

    [<Benchmark(Description = "QT_map2i")>]
    member this.QtMap2i() =
        assignOnOk (map2i qtMatrix1 qtMatrix2 doubleMap2i) (fun r -> resultQt <- r)

    [<Benchmark(Description = "COOLIST_map")>]
    member this.CooListMap() =
        resultList <- COOList.cooMap listMatrix1 doubleMap

    [<Benchmark(Description = "COOLIST_mapi")>]
    member this.CooListMapi() =
        resultList <- COOList.cooMapi listMatrix1 doubleMapi

    [<Benchmark(Description = "COOLIST_map2")>]
    member this.CooListMap2() =
        assignOnOk (COOList.cooMap2 listMatrix1 listMatrix2 op_add) (fun r -> resultList <- r)

    [<Benchmark(Description = "COOLIST_map2i")>]
    member this.CooListMap2i() =
        assignOnOk (COOList.cooMap2i listMatrix1 listMatrix2 doubleMap2i) (fun r -> resultList <- r)

    [<Benchmark(Description = "COOLIST_mxm")>]
    member this.CooListMxm() =
        assignOrFail "COOList mxmcoo failed" (COOList.mxmcoo op_add op_mult listMatrix1 listMatrix1) (fun r ->
            resultList <- r)

    [<Benchmark(Description = "COOLIST_get")>]
    member this.CooListGet() =
        resultListVal <- sumLookups 1000 lookupCoords (fun (i, j) -> COOList.cooGet (listMatrix1, i, j))

    [<Benchmark(Description = "COOLIST_set")>]
    member this.CooListSet() =
        resultList <-
            updateLookups 1000 lookupCoords lookupValues listMatrix1 (fun m i j v -> COOList.cooUpdate (m, i, j, v))

    [<Benchmark(Description = "COO_get")>]
    member this.CooGet() =
        resultCooVal <- sumLookups 1000 lookupCoords (fun (i, j) -> cooGet (cooMatrix1, i, j))

    [<Benchmark(Description = "QT_get")>]
    member this.QtGet() =
        resultQtVal <- sumLookups 1000 lookupCoords (fun (i, j) -> get qtMatrix1 i j)

    [<Benchmark(Description = "COO_set")>]
    member this.CooSet() =
        resultCoo <- updateLookups 1000 lookupCoords lookupValues cooMatrix1 (fun m i j v -> cooUpdate (m, i, j, v))

    [<Benchmark(Description = "QT_set")>]
    member this.QtSet() =
        resultQt <- updateLookups 1000 lookupCoords lookupValues qtMatrix1 (fun m i j v -> set m i j v)

    [<Benchmark(Description = "COO_mxm")>]
    member this.CooMxm() =
        assignOrFail "mxmcoo failed" (mxmcoo op_add op_mult cooMatrix1 cooMatrix1) (fun r -> resultCoo <- r)

    [<Benchmark(Description = "QT_mxm")>]
    member this.QtMxm() =
        assignOrFail "mxm failed" (LinearAlgebra.mxm op_add op_mult qtMatrix1 qtMatrix1) (fun r -> resultQt <- r)


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
    member this.DenseCooMap() = resultCoo <- cooMap cooMatrix doubleMap

    [<Benchmark(Description = "Dense_QT_map")>]
    member this.DenseQtMap() = resultQt <- map qtMatrix doubleMap

    [<Benchmark(Description = "Dense_COO_mapi")>]
    member this.DenseCooMapi() =
        resultCoo <- cooMapi cooMatrix doubleMapi

    [<Benchmark(Description = "Dense_QT_mapi")>]
    member this.DenseQtMapi() = resultQt <- mapi qtMatrix doubleMapi

    [<Benchmark(Description = "Dense_COOLIST_map")>]
    member this.DenseCooListMap() =
        resultList <- COOList.cooMap listMatrix doubleMap

    [<Benchmark(Description = "Dense_COOLIST_mapi")>]
    member this.DenseCooListMapi() =
        resultList <- COOList.cooMapi listMatrix doubleMapi

    [<Benchmark(Description = "Dense_COOLIST_mxm")>]
    member this.DenseCooListMxm() =
        assignOrFail "COOList mxmcoo failed" (COOList.mxmcoo op_add op_mult listMatrix listMatrix) (fun r ->
            resultList <- r)

    [<Benchmark(Description = "Dense_COOLIST_get")>]
    member this.DenseCooListGet() =
        resultListVal <- sumCells this.Size (fun i j -> COOList.cooGet (listMatrix, i, j))

    [<Benchmark(Description = "Dense_COOLIST_set")>]
    member this.DenseCooListSet() =
        resultList <- updateCells this.Size listMatrix (fun m i j v -> COOList.cooUpdate (m, i, j, v))

    [<Benchmark(Description = "Dense_COO_get")>]
    member this.DenseCooGet() =
        resultCoo <- cooMatrix

        ignore (sumCells this.Size (fun i j -> cooGet (cooMatrix, i, j)))

    [<Benchmark(Description = "Dense_QT_get")>]
    member this.DenseQtGet() =
        resultQt <- qtMatrix

        ignore (sumCells this.Size (fun i j -> get qtMatrix (i) (j)))

    [<Benchmark(Description = "Dense_COO_set")>]
    member this.DenseCooSet() =
        resultCoo <- updateCells this.Size cooMatrix (fun m i j v -> cooUpdate (m, i, j, v))

    [<Benchmark(Description = "Dense_QT_set")>]
    member this.DenseQtSet() =
        resultQt <- updateCells this.Size qtMatrix (fun m i j v -> set m (i) (j) v)

    [<Benchmark(Description = "Dense_COO_mxm")>]
    member this.DenseCooMxm() =
        assignOrFail "mxmcoo failed" (mxmcoo op_add op_mult cooMatrix cooMatrix) (fun r -> resultCoo <- r)

    [<Benchmark(Description = "Dense_QT_mxm")>]
    member this.DenseQtMxm() =
        assignOrFail "mxm failed" (LinearAlgebra.mxm op_add op_mult qtMatrix qtMatrix) (fun r -> resultQt <- r)
