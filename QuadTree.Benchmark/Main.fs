open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let benchmarks =
        BenchmarkSwitcher
            [| typeof<QuadTree.Benchmarks.Filters.Benchmark>
               typeof<QuadTree.Benchmarks.Exists.Benchmark>
               typeof<QuadTree.Benchmarks.Forall.Benchmark>
               typeof<QuadTree.Benchmarks.BFS.Benchmark>
               typeof<QuadTree.Benchmarks.SSSP.Benchmark>
               typeof<QuadTree.Benchmarks.Triangles.Benchmark>
               typeof<QuadTree.Benchmarks.Formats.FormatBenchmark>
               typeof<QuadTree.Benchmarks.Formats.DenseFormatBenchmark>
               typeof<QuadTree.Benchmarks.RealMatrices.RealMatrixBenchmark> |]

    benchmarks.Run argv |> ignore
    0
