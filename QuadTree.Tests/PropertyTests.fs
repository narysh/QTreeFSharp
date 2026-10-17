module QuadTree.Tests.PropertyTests

open System
open Xunit
open FsCheck
open FsCheck.FSharp
open FsCheck.Xunit
open Matrix
open COOArray
open OptionMonoid

type Input =
    { Rows: int
      Cols: int
      Cells: (int * int * int) list }

let private fromCoordinateListUnchecked (lst: CoordinateList<'a>) =
    match Matrix.fromCoordinateList lst with
    | Ok m -> m
    | Error e -> failwith e

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
let ``get at every cell agrees between QuadTree and COOArray`` (inp: Input) =
    let coo = toCoo inp
    let cooA = ArrayCOO(coo.nrows, coo.ncols, coo.list)
    let qt = fromCoordinateListUnchecked coo
    let nrows = int (uint64 coo.nrows)
    let ncols = int (uint64 coo.ncols)

    List.allPairs [ 0 .. nrows - 1 ] [ 0 .. ncols - 1 ]
    |> List.forall (fun (r, c) ->
        let ri = uint64 r * 1UL<rowindex>
        let ci = uint64 c * 1UL<colindex>

        Matrix.get qt ri ci = cooGet (cooA, ri, ci))

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``toCoordinateList (fromCoordinateListUnchecked coo) preserves every value`` (inp: Input) =
    let coo = toCoo inp
    let back = toCoordinateList (fromCoordinateListUnchecked coo)
    let backA = ArrayCOO(back.nrows, back.ncols, back.list)

    back.nrows = coo.nrows
    && back.ncols = coo.ncols
    && List.length back.list = List.length coo.list
    && coo.list |> List.forall (fun (r, c, v) -> cooGet (backA, r, c) = Ok(Some v))

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``cooUpdate writes a value and adjusts the length`` (inp: Input) =
    let coo = toCoo inp
    let cooA = ArrayCOO(coo.nrows, coo.ncols, coo.list)
    let nrows = int (uint64 coo.nrows)
    let ncols = int (uint64 coo.ncols)
    let r = abs inp.Rows % nrows
    let c = abs inp.Cols % ncols
    let ri = uint64 r * 1UL<rowindex>
    let ci = uint64 c * 1UL<colindex>
    let wasPresent = cooA.list |> Array.exists (fun (i, j, _) -> i = ri && j = ci)

    match cooUpdate (cooA, ri, ci, 777) with
    | Ok updated ->
        cooGet (updated, ri, ci) = Ok(Some 777)
        && Array.length updated.list = Array.length cooA.list + (if wasPresent then 0 else 1)
    | Error _ -> false

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``set and cooUpdate agree on the written cell`` (inp: Input) =
    let coo = toCoo inp
    let cooA = ArrayCOO(coo.nrows, coo.ncols, coo.list)
    let qt = fromCoordinateListUnchecked coo
    let nrows = int (uint64 coo.nrows)
    let ncols = int (uint64 coo.ncols)
    let r = abs inp.Rows % nrows
    let c = abs inp.Cols % ncols
    let ri = uint64 r * 1UL<rowindex>
    let ci = uint64 c * 1UL<colindex>

    match cooUpdate (cooA, ri, ci, 42), Matrix.set qt ri ci 42 with
    | Ok updatedCoo, Ok updatedQt -> cooGet (updatedCoo, ri, ci) = Matrix.get updatedQt ri ci
    | _ -> false

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``cooMapValues maps every stored value once`` (inp: Input) =
    let coo = toCoo inp
    let cooA = ArrayCOO(coo.nrows, coo.ncols, coo.list)
    let mapped = cooMapValues cooA (fun v -> Some(v + 1))

    Array.length mapped.list = Array.length cooA.list
    && cooA.list
       |> Array.forall (fun (r, c, v) -> cooGet (mapped, r, c) = Ok(Some(v + 1)))

[<Property(Arbitrary = [| typeof<InputArbs> |])>]
let ``out-of-bounds access returns InconsistentSizeOfArguments`` (inp: Input) =
    let coo = toCoo inp
    let cooA = ArrayCOO(coo.nrows, coo.ncols, coo.list)
    let qt = fromCoordinateListUnchecked coo
    let nrows = uint64 coo.nrows * 1UL<rowindex>
    let ncols = uint64 coo.ncols * 1UL<colindex>

    Assert.Equal(Error Error.InconsistentSizeOfArguments, cooGet (cooA, nrows, 0UL<colindex>))
    Assert.Equal(Error Error.InconsistentSizeOfArguments, cooUpdate (cooA, nrows, 0UL<colindex>, 1))
    Assert.Equal(Error Error.InconsistentSizeOfArguments, cooA.[nrows, 0UL<colindex>])

    Assert.Equal(Error Error.InconsistentSizeOfArguments, Matrix.get qt nrows 0UL<colindex>)
    Assert.Equal(Error Error.InconsistentSizeOfArguments, Matrix.set qt nrows 0UL<colindex> 1)

let private cooWithDims (nrows: uint64) (ncols: uint64) (inp: Input) : CoordinateList<int> =
    let entries =
        inp.Cells
        |> List.map (fun (r, c, v) -> (abs r, abs c, v))
        |> List.filter (fun (r, c, _) -> r < int nrows && c < int ncols)
        |> List.distinctBy (fun (r, c, _) -> (r, c))
        |> List.map (fun (r, c, v) -> (uint64 r * 1UL<rowindex>, uint64 c * 1UL<colindex>, v))
        |> List.sortBy (fun (r, c, _) -> (r, c))

    CoordinateList(nrows * 1UL<nrows>, ncols * 1UL<ncols>, entries)

let private naiveMxm (nrowsA: uint64) (k: uint64) (ncolsB: uint64) (m1: COOEntry<int> list) (m2: COOEntry<int> list) =
    let m1Map = m1 |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList
    let m2Map = m2 |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList

    [ for i in 0UL .. nrowsA - 1UL do
          for j in 0UL .. ncolsB - 1UL do
              let products =
                  [ for t in 0UL .. k - 1UL do
                        let a = m1Map |> Map.tryFind (i * 1UL<rowindex>, t * 1UL<colindex>)
                        let b = m2Map |> Map.tryFind (t * 1UL<rowindex>, j * 1UL<colindex>)
                        yield op_mult a b ]

              match products |> List.fold (fun acc p -> op_add acc p) None with
              | Some v -> yield (i * 1UL<rowindex>, j * 1UL<colindex>, v)
              | None -> () ]
    |> List.sortBy (fun (i, j, _) -> (i, j))

let private arbInputPair: Arbitrary<Input * Input> =
    let gen =
        gen {
            let! a = Arb.toGen arbInput
            let! b = Arb.toGen arbInput
            return (a, b)
        }

    Arb.fromGen gen

type InputPairArbs =
    static member InputPair() = arbInputPair

[<Property(Arbitrary = [| typeof<InputPairArbs> |])>]
let ``mxmcoo agrees with naive multiplication, array matches list, keys are sorted and unique``
    ((a, b): Input * Input)
    =
    let nrowsA = uint64 (max 1 a.Rows)
    let k = uint64 (max 1 a.Cols)
    let ncolsB = uint64 (max 1 b.Cols)
    let m1 = cooWithDims nrowsA k a
    let m2 = cooWithDims k ncolsB b
    let m1A = ArrayCOO(m1.nrows, m1.ncols, m1.list)
    let m2A = ArrayCOO(m2.nrows, m2.ncols, m2.list)

    let expected = naiveMxm nrowsA k ncolsB m1.list m2.list

    let sortedUnique entries =
        entries
        |> List.map (fun (i, j, _) -> (i, j))
        |> List.pairwise
        |> List.forall (fun ((i1, j1), (i2, j2)) -> i1 < i2 || (i1 = i2 && j1 < j2))

    match
        COOArray.mxmcoo op_add op_mult m1A m2A,
        COOList.mxmcoo op_add op_mult (COOList.fromArray m1A) (COOList.fromArray m2A)
    with
    | Ok arr, Ok lst ->
        let arrEntries = Array.toList arr.list

        List.indexed expected = List.indexed arrEntries
        && List.indexed expected = List.indexed lst.entries
        && (arrEntries = lst.entries)
        && sortedUnique arrEntries
        && sortedUnique lst.entries
    | _ -> false
