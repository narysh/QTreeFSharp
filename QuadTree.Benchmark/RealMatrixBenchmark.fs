namespace QuadTree.Benchmarks.RealMatrices

open System
open System.IO
open BenchmarkDotNet.Attributes
open Matrix
open COO

[<Config(typeof<QuadTree.Benchmarks.Utils.MyConfig>)>]
[<MemoryDiagnoser>]
type RealMatrixBenchmark() =

    let mutable cooMatrix = Unchecked.defaultof<CoordinateList<double>>
    let mutable qtMatrix = Unchecked.defaultof<SparseMatrix<double>>

    let mutable resultCoo = Unchecked.defaultof<CoordinateList<double>>
    let mutable resultQt = Unchecked.defaultof<SparseMatrix<double>>
    let mutable resultCooVal = 0.0
    let mutable resultQtVal = 0.0

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

            let (coo, qt) = QuadTree.Benchmarks.Utils.readMtxRaw mtxPath (not isSymmetric)

            cooMatrix <- coo
            qtMatrix <- qt

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
            resultCoo <- cooMap cooMatrix (fun v -> v |> Option.map (fun x -> x * 2.0))

    [<Benchmark(Description = "Real_QT_map")>]
    member this.QtMap() =
        if not skip then
            resultQt <- map qtMatrix (fun v -> v |> Option.map (fun x -> x * 2.0))

    [<Benchmark(Description = "Real_COO_mapi")>]
    member this.CooMapi() =
        if not skip then
            resultCoo <-
                cooMapi cooMatrix (fun i j v -> v |> Option.map (fun x -> x + float (uint64 i) + float (uint64 j)))

    [<Benchmark(Description = "Real_QT_mapi")>]
    member this.QtMapi() =
        if not skip then
            resultQt <- mapi qtMatrix (fun i j v -> v |> Option.map (fun x -> x + float (uint64 i) + float (uint64 j)))

    [<Benchmark(Description = "Real_COO_get")>]
    member this.CooGet() =
        if not skip then
            let mutable acc = 0.0

            for k = 0 to lookupCoords.Length - 1 do
                let (i, j) = lookupCoords.[k]

                match cooGet (cooMatrix, i, j) with
                | Ok(Some v) -> acc <- acc + v
                | _ -> ()

            resultCooVal <- acc

    [<Benchmark(Description = "Real_QT_get")>]
    member this.QtGet() =
        if not skip then
            let mutable acc = 0.0

            for k = 0 to lookupCoords.Length - 1 do
                let (i, j) = lookupCoords.[k]

                match get qtMatrix i j with
                | Ok(Some v) -> acc <- acc + v
                | _ -> ()

            resultQtVal <- acc

    [<Benchmark(Description = "Real_COO_set")>]
    member this.CooSet() =
        if not skip then
            let mutable m = cooMatrix

            for k = 0 to lookupCoords.Length - 1 do
                let (i, j) = lookupCoords.[k]

                match cooUpdate (m, i, j, lookupValues.[k] * 2.0) with
                | Ok updated -> m <- updated
                | _ -> ()

            resultCoo <- m

    [<Benchmark(Description = "Real_QT_set")>]
    member this.QtSet() =
        if not skip then
            let mutable m = qtMatrix

            for k = 0 to lookupCoords.Length - 1 do
                let (i, j) = lookupCoords.[k]

                match set m i j (lookupValues.[k] * 2.0) with
                | Ok updated -> m <- updated
                | _ -> ()

            resultQt <- m

    [<Benchmark(Description = "Real_COO_mxm")>]
    member this.CooMxm() =
        if not skip && doMxm then
            let op_add x y =
                match x, y with
                | Some a, Some b -> Some(a + b)
                | Some a, None
                | None, Some a -> Some a
                | None, None -> None

            let op_mult x y =
                match x, y with
                | Some a, Some b -> Some(a * b)
                | _ -> None

            match mxmcoo op_add op_mult cooMatrix cooMatrix with
            | Ok result -> resultCoo <- result
            | Error _ -> failwith "mxmcoo failed"

    [<Benchmark(Description = "Real_QT_mxm")>]
    member this.QtMxm() =
        if not skip && doMxm then
            let op_add x y =
                match x, y with
                | Some a, Some b -> Some(a + b)
                | Some a, None
                | None, Some a -> Some a
                | None, None -> None

            let op_mult x y =
                match x, y with
                | Some a, Some b -> Some(a * b)
                | _ -> None

            match LinearAlgebra.mxm op_add op_mult qtMatrix qtMatrix with
            | Ok result -> resultQt <- result
            | Error _ -> failwith "mxm failed"
