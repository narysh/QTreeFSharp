module QuadTree.Tests.PropertyTests

open System
open Xunit
open FsCheck
open FsCheck.FSharp
open FsCheck.Xunit
open Matrix
open COO

type Input =
    { Rows: int
      Cols: int
      Cells: (int * int * int) list }

let private toCoo (inp: Input) : CoordinateList<int> =
    let nrows = max 1 inp.Rows
    let ncols = max 1 inp.Cols

    let entries =
        inp.Cells
        |> List.map (fun (r, c, v) -> (abs r, abs c, v))
        |> List.filter (fun (r, c, _) -> r < nrows && c < ncols)
        |> List.distinctBy (fun (r, c, _) -> (r, c))
        |> List.map (fun (r, c, v) -> (uint64 r * 1UL<rowindex>, uint64 c * 1UL<colindex>, v))
        |> List.sortBy (fun (r, c, _) -> (r, c))

    CoordinateList(uint64 nrows * 1UL<nrows>, uint64 ncols * 1UL<ncols>, entries)

let private arbInput: Arbitrary<Input> =
    let gen =
        gen {
            let! rows = Gen.choose (1, 16)
            let! cols = Gen.choose (1, 16)

            let! cells =
                Gen.listOf (
                    gen {
                        let! r = Gen.choose (-5, 20)
                        let! c = Gen.choose (-5, 20)
                        let! v = Gen.choose (-100, 100)
                        return (r, c, v)
                    }
                )

            return
                { Rows = rows
                  Cols = cols
                  Cells = cells }
        }

    Arb.fromGen gen

type InputArbs =
    static member Input() = arbInput

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``get at every cell agrees between QuadTree and COO`` (inp: Input) =
    let coo = toCoo inp
    let qt = fromCoordinateList coo
    let nrows = int (uint64 coo.nrows)
    let ncols = int (uint64 coo.ncols)

    List.allPairs [ 0 .. nrows - 1 ] [ 0 .. ncols - 1 ]
    |> List.forall (fun (r, c) ->
        let ri = uint64 r * 1UL<rowindex>
        let ci = uint64 c * 1UL<colindex>

        Matrix.get qt ri ci = cooGet (coo, ri, ci))

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``toCoordinateList (fromCoordinateList coo) preserves every value`` (inp: Input) =
    let coo = toCoo inp
    let back = toCoordinateList (fromCoordinateList coo)

    back.nrows = coo.nrows
    && back.ncols = coo.ncols
    && Array.length back.list = Array.length coo.list
    && coo.list |> Array.forall (fun (r, c, v) -> cooGet (back, r, c) = Ok(Some v))

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``cooUpdate writes a value and adjusts the length`` (inp: Input) =
    let coo = toCoo inp
    let nrows = int (uint64 coo.nrows)
    let ncols = int (uint64 coo.ncols)
    let r = abs inp.Rows % nrows
    let c = abs inp.Cols % ncols
    let ri = uint64 r * 1UL<rowindex>
    let ci = uint64 c * 1UL<colindex>
    let wasPresent = coo.list |> Array.exists (fun (i, j, _) -> i = ri && j = ci)

    match cooUpdate (coo, ri, ci, 777) with
    | Ok updated ->
        cooGet (updated, ri, ci) = Ok(Some 777)
        && Array.length updated.list = Array.length coo.list + (if wasPresent then 0 else 1)
    | Error _ -> false

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``set and cooUpdate agree on the written cell`` (inp: Input) =
    let coo = toCoo inp
    let qt = fromCoordinateList coo
    let nrows = int (uint64 coo.nrows)
    let ncols = int (uint64 coo.ncols)
    let r = abs inp.Rows % nrows
    let c = abs inp.Cols % ncols
    let ri = uint64 r * 1UL<rowindex>
    let ci = uint64 c * 1UL<colindex>

    match cooUpdate (coo, ri, ci, 42), Matrix.set qt ri ci 42 with
    | Ok updatedCoo, Ok updatedQt -> cooGet (updatedCoo, ri, ci) = Matrix.get updatedQt ri ci
    | _ -> false

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``cooMapValues maps every stored value once`` (inp: Input) =
    let coo = toCoo inp
    let mapped = cooMapValues coo (fun v -> Some(v + 1))

    Array.length mapped.list = Array.length coo.list
    && coo.list
       |> Array.forall (fun (r, c, v) -> cooGet (mapped, r, c) = Ok(Some(v + 1)))

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``out-of-bounds access raises ArgumentOutOfRangeException`` (inp: Input) =
    let coo = toCoo inp
    let qt = fromCoordinateList coo
    let nrows = uint64 coo.nrows * 1UL<rowindex>
    let ncols = uint64 coo.ncols * 1UL<colindex>

    let cooGetThrows =
        try
            cooGet (coo, nrows, 0UL<colindex>) |> ignore
            false
        with :? ArgumentOutOfRangeException ->
            true

    let cooUpdateThrows =
        try
            cooUpdate (coo, nrows, 0UL<colindex>, 1) |> ignore
            false
        with :? ArgumentOutOfRangeException ->
            true

    let matrixGetThrows =
        try
            Matrix.get qt nrows 0UL<colindex> |> ignore
            false
        with :? ArgumentOutOfRangeException ->
            true

    let matrixSetThrows =
        try
            Matrix.set qt nrows 0UL<colindex> 1 |> ignore
            false
        with :? ArgumentOutOfRangeException ->
            true

    cooGetThrows && cooUpdateThrows && matrixGetThrows && matrixSetThrows
