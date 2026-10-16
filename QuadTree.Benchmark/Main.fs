open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let benchmarks =
        BenchmarkSwitcher
            [| typeof<QuadTree.Benchmarks.BFS.Benchmark>
               typeof<QuadTree.Benchmarks.SSSP.Benchmark>
               typeof<QuadTree.Benchmarks.Triangles.Benchmark>
               typeof<QuadTree.Benchmarks.ReduceComparison.Benchmark>
               typeof<QuadTree.Benchmarks.VectorSlice.Benchmark>
               typeof<QuadTree.Benchmarks.MatrixSlice.Benchmark>
               typeof<QuadTree.Benchmarks.Kronecker.Benchmark>
               typeof<QuadTree.Benchmarks.MatrixSliceAlign.Benchmark>
               typeof<QuadTree.Benchmarks.VectorSliceAlign.Benchmark>
               typeof<QuadTree.Benchmarks.Formats.FormatBenchmark>
               typeof<QuadTree.Benchmarks.Formats.DenseFormatBenchmark>
               typeof<QuadTree.Benchmarks.RealMatrices.RealMatrixBenchmark>
               typeof<QuadTree.Benchmarks.AVLSet.SingleOpsBenchmark>
               typeof<QuadTree.Benchmarks.AVLSet.TraversalSetsBenchmark>
               typeof<QuadTree.Benchmarks.AVLSet.ParallelSetsBenchmark>
               typeof<QuadTree.Benchmarks.AVLSet.FSSetsBenchmark>
               typeof<QuadTree.Benchmarks.AVLSet.HashSetBenchmark> |]

    benchmarks.Run argv |> ignore
    0
