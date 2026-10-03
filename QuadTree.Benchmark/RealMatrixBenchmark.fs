namespace QuadTree.Benchmarks.RealMatrices

open System
open System.IO
open BenchmarkDotNet.Attributes
open Matrix
open COOArray
open QuadTree.Benchmarks.Utils

[<Config(typeof<QuadTree.Benchmarks.Utils.MyConfig>)>]
[<MemoryDiagnoser>]
type RealMatrixBenchmark() =

    let mutable cooMatrix = Unchecked.defaultof<ArrayCOO<double>>
    let mutable qtMatrix = Unchecked.defaultof<SparseMatrix<double>>
    let mutable listMatrix = Unchecked.defaultof<COOList.ListCOO<double>>

    let mutable resultCoo = Unchecked.defaultof<ArrayCOO<double>>
    let mutable resultQt = Unchecked.defaultof<SparseMatrix<double>>
    let mutable resultList = Unchecked.defaultof<COOList.ListCOO<double>>
    let mutable resultCooVal = 0.0
    let mutable resultQtVal = 0.0
    let mutable resultListVal = 0.0

    let mutable lookupCoords: (uint64<rowindex> * uint64<colindex>) array = [||]
    let mutable lookupValues: double array = [||]

    let mutable matrixName = ""
    let mutable doMxm = false
    let mutable skip = false

    [<Params("494_bus",
             "arc130",
             "bcspwr01",
             "bcspwr05",
             "bcspwr06",
             "cryg2500",
             "dwt_992",
             "jagmesh7",
             "west0479",
             "zenios")>]
    member val MatrixName = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        matrixName <- this.MatrixName
        skip <- false
        let dataDir = Path.GetFullPath(QuadTree.Benchmarks.Utils.DIR_WITH_MATRICES)
        let mtxPath = Path.Combine(dataDir, matrixName + ".mtx")

        if not (File.Exists mtxPath) then
            skip <- true
        else
            let isSymmetric =
                File.ReadLines(mtxPath)
                |> Seq.exists (fun s -> s.StartsWith "%%MatrixMarket" && s.Contains "symmetric")

            let (coo, qtResult) = QuadTree.Benchmarks.Utils.readMtxRaw mtxPath (not isSymmetric)

            cooMatrix <- new ArrayCOO<double>(coo.nrows, coo.ncols, coo.list)

            qtMatrix <-
                match qtResult with
                | Ok m -> m
                | Error e -> failwithf "fromCoordinateList failed: %s" e

            listMatrix <- COOList.fromArray cooMatrix

            let nnz = coo.list.Length
            let dim = max (uint64 coo.nrows) (uint64 coo.ncols)
            doMxm <- nnz < 100000 && dim <= 12119UL

            let rng = Random(42)
            let sampleSize = min nnz 1000
            let coords = coo.list
            let indices = Array.init sampleSize (fun _ -> rng.Next(nnz))

            lookupCoords <-
                indices
                |> Array.map (fun k ->
                    let (i, j, _) = coords.[k]
                    (i, j))

            lookupValues <-
                indices
                |> Array.map (fun k ->
                    let (_, _, v) = coords.[k]
                    v)

    [<Benchmark(Baseline = true, Description = "Real_COO_map")>]
    member this.CooMap() =
        if not skip then
            resultCoo <- cooMap cooMatrix doubleMap

    [<Benchmark(Description = "Real_QT_map")>]
    member this.QtMap() =
        if not skip then
            resultQt <- map qtMatrix doubleMap

    [<Benchmark(Description = "Real_COO_mapi")>]
    member this.CooMapi() =
        if not skip then
            resultCoo <- cooMapi cooMatrix doubleMapi

    [<Benchmark(Description = "Real_QT_mapi")>]
    member this.QtMapi() =
        if not skip then
            resultQt <- mapi qtMatrix doubleMapi

    [<Benchmark(Description = "Real_COOLIST_map")>]
    member this.CooListMap() =
        if not skip then
            resultList <- COOList.cooMap listMatrix doubleMap

    [<Benchmark(Description = "Real_COOLIST_mapi")>]
    member this.CooListMapi() =
        if not skip then
            resultList <- COOList.cooMapi listMatrix doubleMapi

    [<Benchmark(Description = "Real_COOLIST_mxm")>]
    member this.CooListMxm() =
        if not skip && doMxm then
            assignOrFail "COOList mxmcoo failed" (COOList.mxmcoo op_add op_mult listMatrix listMatrix) (fun r ->
                resultList <- r)

    [<Benchmark(Description = "Real_COOLIST_get")>]
    member this.CooListGet() =
        if not skip then
            resultListVal <-
                sumLookups lookupCoords.Length lookupCoords (fun (i, j) -> COOList.cooGet (listMatrix, i, j))

    [<Benchmark(Description = "Real_COOLIST_set")>]
    member this.CooListSet() =
        if not skip then
            resultList <-
                updateLookups lookupCoords.Length lookupCoords lookupValues listMatrix (fun m i j v ->
                    COOList.cooUpdate (m, i, j, v))

    [<Benchmark(Description = "Real_COO_get")>]
    member this.CooGet() =
        if not skip then
            resultCooVal <- sumLookups lookupCoords.Length lookupCoords (fun (i, j) -> cooGet (cooMatrix, i, j))

    [<Benchmark(Description = "Real_QT_get")>]
    member this.QtGet() =
        if not skip then
            resultQtVal <- sumLookups lookupCoords.Length lookupCoords (fun (i, j) -> get qtMatrix i j)

    [<Benchmark(Description = "Real_COO_set")>]
    member this.CooSet() =
        if not skip then
            resultCoo <-
                updateLookups lookupCoords.Length lookupCoords lookupValues cooMatrix (fun m i j v ->
                    cooUpdate (m, i, j, v))

    [<Benchmark(Description = "Real_QT_set")>]
    member this.QtSet() =
        if not skip then
            resultQt <-
                updateLookups lookupCoords.Length lookupCoords lookupValues qtMatrix (fun m i j v -> set m i j v)

    [<Benchmark(Description = "Real_COO_mxm")>]
    member this.CooMxm() =
        if not skip && doMxm then
            assignOrFail "mxmcoo failed" (mxmcoo op_add op_mult cooMatrix cooMatrix) (fun r -> resultCoo <- r)

    [<Benchmark(Description = "Real_QT_mxm")>]
    member this.QtMxm() =
        if not skip && doMxm then
            assignOrFail "mxm failed" (LinearAlgebra.mxm op_add op_mult qtMatrix qtMatrix) (fun r -> resultQt <- r)
