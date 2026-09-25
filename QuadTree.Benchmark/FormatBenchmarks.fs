namespace QuadTree.Benchmarks.Formats

open System
open BenchmarkDotNet.Attributes
open Matrix
open COO

[<Config(typeof<QuadTree.Benchmarks.Utils.MyConfig>)>]
type FormatBenchmark() =

    let mutable cooMatrix1 = Unchecked.defaultof<CoordinateList<double>>
    let mutable cooMatrix2 = Unchecked.defaultof<CoordinateList<double>>
    let mutable qtMatrix1 = Unchecked.defaultof<SparseMatrix<double>>
    let mutable qtMatrix2 = Unchecked.defaultof<SparseMatrix<double>>

    let mutable lookupCoords: (uint64<rowindex> * uint64<colindex>) array = [||]
    let mutable lookupValues: double array = [||]

    let mutable resultCoo = Unchecked.defaultof<CoordinateList<double>>
    let mutable resultQt = Unchecked.defaultof<SparseMatrix<double>>
    let mutable resultCooVal = 0.0
    let mutable resultQtVal = 0.0

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

        cooMatrix1 <- CoordinateList(size * 1UL<nrows>, size * 1UL<ncols>, entries1)
        cooMatrix2 <- CoordinateList(size * 1UL<nrows>, size * 1UL<ncols>, entries2)
        qtMatrix1 <- fromCoordinateList cooMatrix1
        qtMatrix2 <- fromCoordinateList cooMatrix2

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
        match
            cooMap2 cooMatrix1 cooMatrix2 (fun a b ->
                match a, b with
                | Some x, Some y -> Some(x + y)
                | Some x, None -> Some x
                | None, Some y -> Some y
                | None, None -> None)
        with
        | Ok r -> resultCoo <- r
        | Error _ -> ()

    [<Benchmark(Description = "QT_map2")>]
    member this.QtMap2() =
        match
            map2 qtMatrix1 qtMatrix2 (fun a b ->
                match a, b with
                | Some x, Some y -> Some(x + y)
                | Some x, None -> Some x
                | None, Some y -> Some y
                | None, None -> None)
        with
        | Ok r -> resultQt <- r
        | Error _ -> ()

    [<Benchmark(Description = "COO_map2i")>]
    member this.CooMap2i() =
        match
            cooMap2i cooMatrix1 cooMatrix2 (fun i j a b ->
                match a, b with
                | Some x, Some y -> Some(x + y + float (uint64 i))
                | Some x, None -> Some x
                | None, Some y -> Some y
                | None, None -> None)
        with
        | Ok r -> resultCoo <- r
        | Error _ -> ()

    [<Benchmark(Description = "QT_map2i")>]
    member this.QtMap2i() =
        match
            map2i qtMatrix1 qtMatrix2 (fun i j a b ->
                match a, b with
                | Some x, Some y -> Some(x + y + float (uint64 i))
                | Some x, None -> Some x
                | None, Some y -> Some y
                | None, None -> None)
        with
        | Ok r -> resultQt <- r
        | Error _ -> ()

    [<Benchmark(Description = "COO_get")>]
    member this.CooGet() =
        let n = min lookupCoords.Length 1000
        let mutable acc = 0.0

        for k = 0 to n - 1 do
            let (i, j) = lookupCoords.[k]

            match cooGet (cooMatrix1, i, j) with
            | Ok(Some v) -> acc <- acc + v
            | _ -> ()

        resultCooVal <- acc

    [<Benchmark(Description = "QT_get")>]
    member this.QtGet() =
        let n = min lookupCoords.Length 1000
        let mutable acc = 0.0

        for k = 0 to n - 1 do
            let (i, j) = lookupCoords.[k]

            match get qtMatrix1 i j with
            | Ok(Some v) -> acc <- acc + v
            | _ -> ()

        resultQtVal <- acc

    [<Benchmark(Description = "COO_set")>]
    member this.CooSet() =
        let n = min lookupCoords.Length 1000
        let mutable m = cooMatrix1

        for k = 0 to n - 1 do
            let (i, j) = lookupCoords.[k]

            match cooUpdate (m, i, j, lookupValues.[k] * 2.0) with
            | Ok updated -> m <- updated
            | _ -> ()

        resultCoo <- m

    [<Benchmark(Description = "QT_set")>]
    member this.QtSet() =
        let n = min lookupCoords.Length 1000
        let mutable m = qtMatrix1

        for k = 0 to n - 1 do
            let (i, j) = lookupCoords.[k]

            match set m i j (lookupValues.[k] * 2.0) with
            | Ok updated -> m <- updated
            | _ -> ()

        resultQt <- m

    [<Benchmark(Description = "COO_mxm")>]
    member this.CooMxm() =
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

        match mxmcoo op_add op_mult cooMatrix1 cooMatrix1 with
        | Ok result -> resultCoo <- result
        | Error _ -> failwith "mxmcoo failed"

    [<Benchmark(Description = "QT_mxm")>]
    member this.QtMxm() =
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

        match LinearAlgebra.mxm op_add op_mult qtMatrix1 qtMatrix1 with
        | Ok result -> resultQt <- result
        | Error _ -> failwith "mxm failed"


[<Config(typeof<QuadTree.Benchmarks.Utils.MyConfig>)>]
type DenseFormatBenchmark() =

    let mutable cooMatrix = Unchecked.defaultof<CoordinateList<double>>
    let mutable qtMatrix = Unchecked.defaultof<SparseMatrix<double>>

    let mutable resultCoo = Unchecked.defaultof<CoordinateList<double>>
    let mutable resultQt = Unchecked.defaultof<SparseMatrix<double>>

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

        cooMatrix <- CoordinateList(size * 1UL<nrows>, size * 1UL<ncols>, entries)
        qtMatrix <- fromCoordinateList cooMatrix

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

    [<Benchmark(Description = "Dense_QT_mxm")>]
    member this.DenseQtMxm() =
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
