module COOArray.Tests

open OptionMonoid

open System
open Xunit

open Matrix
open COOArray
open Common


open COOList

type COO<'T> =
    | Array of ArrayCOO<'T>
    | List of ListCOO<'T>

module Formats =
    let create isArray r c l : COO<'T> =
        if isArray then
            COO.Array(ArrayCOO(r, c, l))
        else
            COO.List(ListCOO(r, c, l))

    let toArray<'T> _isArray (m: COO<'T>) : COOEntry<'T> array =
        match m with
        | COO.Array a -> a.list
        | COO.List l -> l.entries |> Array.ofList

    let toList<'T> _isArray (m: COO<'T>) : COOEntry<'T> list =
        match m with
        | COO.Array a -> a.list |> Array.toList
        | COO.List l -> l.entries

    let nrows _isArray (m: COO<'T>) =
        match m with
        | COO.Array a -> a.nrows
        | COO.List l -> l.nrows

    let ncols _isArray (m: COO<'T>) =
        match m with
        | COO.Array a -> a.ncols
        | COO.List l -> l.ncols

    let cooMap<'T, 'U> _isArray (m: COO<'T>) f : COO<'U> =
        match m with
        | COO.Array a -> COO.Array(COOArray.cooMap a f)
        | COO.List l -> COO.List(COOList.cooMap l f)

    let cooMapValues<'T, 'U> _isArray (m: COO<'T>) f : COO<'U> =
        match m with
        | COO.Array a -> COO.Array(COOArray.cooMapValues a f)
        | COO.List l -> COO.List(COOList.cooMapValues l f)

    let cooMapi<'T, 'U> _isArray (m: COO<'T>) f : COO<'U> =
        match m with
        | COO.Array a -> COO.Array(COOArray.cooMapi a f)
        | COO.List l -> COO.List(COOList.cooMapi l f)

    let cooMapiValues<'T, 'U> _isArray (m: COO<'T>) f : COO<'U> =
        match m with
        | COO.Array a -> COO.Array(COOArray.cooMapiValues a f)
        | COO.List l -> COO.List(COOList.cooMapiValues l f)

    let cooMap2<'T, 'U, 'V when 'T: equality and 'U: equality>
        _isArray
        (m1: COO<'T>)
        (m2: COO<'U>)
        f
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.cooMap2 a1 a2 f |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.cooMap2 l1 l2 f |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let cooMap2Values<'T, 'U, 'V when 'T: equality and 'U: equality>
        _isArray
        (m1: COO<'T>)
        (m2: COO<'U>)
        f
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.cooMap2Values a1 a2 f |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.cooMap2Values l1 l2 f |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let cooMap2LeftValues<'T, 'U, 'V when 'T: equality and 'U: equality>
        _isArray
        (m1: COO<'T>)
        (m2: COO<'U>)
        f
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.cooMap2LeftValues a1 a2 f |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.cooMap2LeftValues l1 l2 f |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let cooMap2AllCells<'T, 'U, 'V when 'T: equality and 'U: equality>
        _isArray
        (m1: COO<'T>)
        (m2: COO<'U>)
        f
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.cooMap2AllCells a1 a2 f |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.cooMap2AllCells l1 l2 f |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let cooMap2AtLeastOne<'T, 'U, 'V when 'T: equality and 'U: equality>
        _isArray
        (m1: COO<'T>)
        (m2: COO<'U>)
        f
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.cooMap2AtLeastOne a1 a2 f |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.cooMap2AtLeastOne l1 l2 f |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let cooMap2i<'T, 'U, 'V when 'T: equality and 'U: equality>
        _isArray
        (m1: COO<'T>)
        (m2: COO<'U>)
        f
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.cooMap2i a1 a2 f |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.cooMap2i l1 l2 f |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let cooMap2iValues<'T, 'U, 'V when 'T: equality and 'U: equality>
        _isArray
        (m1: COO<'T>)
        (m2: COO<'U>)
        f
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.cooMap2iValues a1 a2 f |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.cooMap2iValues l1 l2 f |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let cooMap2iLeftValues<'T, 'U, 'V when 'T: equality and 'U: equality>
        _isArray
        (m1: COO<'T>)
        (m2: COO<'U>)
        f
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.cooMap2iLeftValues a1 a2 f |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.cooMap2iLeftValues l1 l2 f |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let cooMap2iAllCells<'T, 'U, 'V when 'T: equality and 'U: equality>
        _isArray
        (m1: COO<'T>)
        (m2: COO<'U>)
        f
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.cooMap2iAllCells a1 a2 f |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.cooMap2iAllCells l1 l2 f |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let cooMap2iAtLeastOne<'T, 'U, 'V when 'T: equality and 'U: equality>
        _isArray
        (m1: COO<'T>)
        (m2: COO<'U>)
        f
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.cooMap2iAtLeastOne a1 a2 f |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.cooMap2iAtLeastOne l1 l2 f |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let mxmcoo<'T, 'U, 'V when 'T: equality and 'U: equality and 'V: equality>
        _isArray
        (op1: 'V option -> 'V option -> 'V option)
        (op2: 'T option -> 'U option -> 'V option)
        (m1: COO<'T>)
        (m2: COO<'U>)
        : Result<COO<'V>, Error> =
        match m1, m2 with
        | COO.Array a1, COO.Array a2 -> COOArray.mxmcoo op1 op2 a1 a2 |> Result.map COO.Array
        | COO.List l1, COO.List l2 -> COOList.mxmcoo op1 op2 l1 l2 |> Result.map COO.List
        | _ -> Error Error.InconsistentSizeOfArguments

    let cooGet<'T> _isArray (m: COO<'T>, i, j) =
        match m with
        | COO.Array a -> COOArray.cooGet (a, i, j)
        | COO.List l -> COOList.cooGet (l, i, j)

    let cooUpdate<'T when 'T: equality> _isArray (m: COO<'T>, i, j, v) : Result<COO<'T>, Error> =
        match m with
        | COO.Array a -> COOArray.cooUpdate (a, i, j, v) |> Result.map COO.Array
        | COO.List l -> COOList.cooUpdate (l, i, j, v) |> Result.map COO.List




let private keysAscending (entries: COOEntry<'v> list) =
    entries
    |> List.map (fun (i, j, _) -> (i, j))
    |> List.pairwise
    |> List.forall (fun ((a1, b1), (a2, b2)) -> a1 < a2 || (a1 = a2 && b1 < b2))

// === Formats.cooGet<int> isArray tests ===

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooGet<int> isArray existing value`` (isArray: bool) =
    let coo =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1)
               (0UL<rowindex>, 1UL<colindex>, 2)
               (1UL<rowindex>, 0UL<colindex>, 3) ])

    let actual = Formats.cooGet<int> isArray (coo, 0UL<rowindex>, 1UL<colindex>)

    Assert.Equal(Ok(Some 2), actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooGet<int> isArray missing value`` (isArray: bool) =
    let coo =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let actual = Formats.cooGet<int> isArray (coo, 2UL<rowindex>, 2UL<colindex>)

    Assert.Equal(Ok None, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooGet<int> isArray out of bounds`` (isArray: bool) =
    let coo =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([ (0UL<rowindex>, 0UL<colindex>, 1) ])

    Assert.Throws<System.ArgumentOutOfRangeException>(fun () ->
        Formats.cooGet<int> isArray (coo, 5UL<rowindex>, 5UL<colindex>) |> ignore)

// === Formats.cooUpdate isArray tests ===

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooUpdate isArray replaces existing`` (isArray: bool) =
    let coo =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let expected =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 99); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let actual = Formats.cooUpdate isArray (coo, 0UL<rowindex>, 0UL<colindex>, 99)

    Assert.Equal(Ok expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooUpdate isArray inserts new in middle`` (isArray: bool) =
    let coo =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 2) ])

    let expected =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1)
               (1UL<rowindex>, 1UL<colindex>, 10)
               (2UL<rowindex>, 2UL<colindex>, 2) ])

    let actual = Formats.cooUpdate isArray (coo, 1UL<rowindex>, 1UL<colindex>, 10)

    Assert.Equal(Ok expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooUpdate isArray inserts at end`` (isArray: bool) =
    let coo =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([ (0UL<rowindex>, 0UL<colindex>, 1) ])

    let expected =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1); (3UL<rowindex>, 3UL<colindex>, 20) ])

    let actual = Formats.cooUpdate isArray (coo, 3UL<rowindex>, 3UL<colindex>, 20)

    Assert.Equal(Ok expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooUpdate isArray out of bounds`` (isArray: bool) =
    let coo =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([ (0UL<rowindex>, 0UL<colindex>, 1) ])

    Assert.Throws<System.ArgumentOutOfRangeException>(fun () ->
        Formats.cooUpdate isArray (coo, 5UL<rowindex>, 5UL<colindex>, 99) |> ignore)

// === Formats.cooMap<int, int> isArray tests ===

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap<int, int> isArray doubles values`` (isArray: bool) =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 2)
          (1UL<rowindex>, 0UL<colindex>, 3)
          (1UL<rowindex>, 1UL<colindex>, 4) ]
        |> List.sort

    let coo = Formats.create isArray nrows ncols (data)

    let f v = v |> Option.map (fun v -> v * 2)

    let expected =
        Formats.create
            isArray
            nrows
            ncols
            ([ (0UL<rowindex>, 0UL<colindex>, 2)
               (0UL<rowindex>, 1UL<colindex>, 4)
               (1UL<rowindex>, 0UL<colindex>, 6)
               (1UL<rowindex>, 1UL<colindex>, 8) ])

    let actual = Formats.cooMap<int, int> isArray coo f

    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap<int, int> isArray filters None results`` (isArray: bool) =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 2)
          (1UL<rowindex>, 0UL<colindex>, 3)
          (1UL<rowindex>, 1UL<colindex>, 4) ]

    let coo = Formats.create isArray nrows ncols (data)

    let f v =
        v
        |> Option.bind (fun v ->
            match v with
            | 1 -> None
            | _ -> Some(v * 10))

    let expected =
        Formats.create
            isArray
            nrows
            ncols
            ([ (0UL<rowindex>, 1UL<colindex>, 20)
               (1UL<rowindex>, 0UL<colindex>, 30)
               (1UL<rowindex>, 1UL<colindex>, 40) ])

    let actual = Formats.cooMap<int, int> isArray coo f

    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap<int, int> isArray and Formats.cooMapi<int, int> isArray fill missing cells (general form)``
    (isArray: bool)
    =
    let nrows = 3UL<nrows>
    let ncols = 3UL<ncols>

    let data = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 5) ]

    let coo = Formats.create isArray nrows ncols (data)

    let f v = Some(defaultArg v 0)

    for actual in
        [ Formats.cooMap<int, int> isArray coo f
          Formats.cooMapi<int, int> isArray coo (fun _ _ v -> f v) ] do
        Assert.Equal(nrows, (Formats.nrows isArray actual))
        Assert.Equal(ncols, (Formats.ncols isArray actual))
        Assert.Equal(9, (Formats.toArray<int> isArray actual).Length)

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 2UL<rowindex> && j = 2UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(2UL<rowindex>, 2UL<colindex>, 5)
        )

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap<int, int> isArray and Formats.cooMapi<int, int> isArray on zero-size matrix`` (isArray: bool) =
    let coo =
        Formats.create isArray 0UL<nrows> 0UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    let f v = v |> Option.map (fun v -> v * 2)

    let expected =
        Formats.create isArray 0UL<nrows> 0UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    Assert.Equal(expected, Formats.cooMap<int, int> isArray coo f)
    Assert.Equal(expected, Formats.cooMapi<int, int> isArray coo (fun _ _ v -> f v))

// === Formats.cooMap2<int, int, int> isArray tests ===

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2<int, int, int> isArray addition`` (isArray: bool) =
    let nrows = 10UL<nrows>
    let ncols = 12UL<ncols>

    let d1 =
        [ (0UL<rowindex>, 3UL<colindex>, 4)
          (3UL<rowindex>, 11UL<colindex>, 2)
          (9UL<rowindex>, 2UL<colindex>, 5) ]
        |> List.sort

    let d2 =
        [ (0UL<rowindex>, 3UL<colindex>, 6)
          (3UL<rowindex>, 3UL<colindex>, 33)
          (3UL<rowindex>, 11UL<colindex>, -1) ]
        |> List.sort

    let f x y =
        match x, y with
        | Some a, Some b -> Some(a + b)
        | Some a, None -> Some a
        | None, Some b -> Some b
        | _ -> None

    let expected =
        Formats.create
            isArray
            nrows
            ncols
            ([ (0UL<rowindex>, 3UL<colindex>, 10)
               (3UL<rowindex>, 3UL<colindex>, 33)
               (9UL<rowindex>, 2UL<colindex>, 5)
               (3UL<rowindex>, 11UL<colindex>, 1) ]
             |> List.sort)

    let c1 = Formats.create isArray nrows ncols (d1)
    let c2 = Formats.create isArray nrows ncols (d2)

    let actual = Formats.cooMap2<int, int, int> isArray c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2<int, int, int> isArray with mismatched positions`` (isArray: bool) =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 3) ]

    let d2 = [ (1UL<rowindex>, 1UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 30) ]

    let f x y =
        match x, y with
        | Some a, Some b -> Some(a + b)
        | Some a, None -> Some(a + 100)
        | None, Some b -> Some(b + 200)
        | _ -> None

    let expected =
        Formats.create
            isArray
            nrows
            ncols
            ([ (0UL<rowindex>, 0UL<colindex>, 101)
               (1UL<rowindex>, 1UL<colindex>, 210)
               (2UL<rowindex>, 2UL<colindex>, 103)
               (3UL<rowindex>, 3UL<colindex>, 230) ])

    let c1 = Formats.create isArray nrows ncols (d1)
    let c2 = Formats.create isArray nrows ncols (d2)

    let actual = Formats.cooMap2<int, int, int> isArray c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2<int, int, int> isArray dense filters None from existing entries`` (isArray: bool) =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
    let d2 = [ (0UL<rowindex>, 0UL<colindex>, 10); (2UL<rowindex>, 2UL<colindex>, 30) ]

    let f x y =
        match x, y with
        | Some a, Some b when a + b > 5 -> None
        | Some a, Some b -> Some(a + b)
        | Some a, None -> Some a
        | None, Some b -> Some b
        | None, None -> Some 0

    let expected =
        Formats.create
            isArray
            nrows
            ncols
            ([ (0UL<rowindex>, 1UL<colindex>, 0)
               (0UL<rowindex>, 2UL<colindex>, 0)
               (0UL<rowindex>, 3UL<colindex>, 0)
               (1UL<rowindex>, 0UL<colindex>, 0)
               (1UL<rowindex>, 1UL<colindex>, 2)
               (1UL<rowindex>, 2UL<colindex>, 0)
               (1UL<rowindex>, 3UL<colindex>, 0)
               (2UL<rowindex>, 0UL<colindex>, 0)
               (2UL<rowindex>, 1UL<colindex>, 0)
               (2UL<rowindex>, 2UL<colindex>, 30)
               (2UL<rowindex>, 3UL<colindex>, 0)
               (3UL<rowindex>, 0UL<colindex>, 0)
               (3UL<rowindex>, 1UL<colindex>, 0)
               (3UL<rowindex>, 2UL<colindex>, 0)
               (3UL<rowindex>, 3UL<colindex>, 0) ])

    let c1 = Formats.create isArray nrows ncols (d1)
    let c2 = Formats.create isArray nrows ncols (d2)

    let actual = Formats.cooMap2<int, int, int> isArray c1 c2 f

    Assert.Equal(Ok expected, actual)

// === Formats.cooMapi<int, int> isArray tests ===

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMapi<int, int> isArray position-dependent values`` (isArray: bool) =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (1UL<rowindex>, 1UL<colindex>, 2)
          (2UL<rowindex>, 3UL<colindex>, 3) ]
        |> List.sort

    let coo = Formats.create isArray nrows ncols (data)

    let f i j v =
        v |> Option.map (fun v -> v + (int (uint64 i)))

    let expected =
        Formats.create
            isArray
            nrows
            ncols
            ([ (0UL<rowindex>, 0UL<colindex>, 1)
               (1UL<rowindex>, 1UL<colindex>, 3)
               (2UL<rowindex>, 3UL<colindex>, 5) ])

    let actual = Formats.cooMapi<int, int> isArray coo f

    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMapi<int, int> isArray filters None results`` (isArray: bool) =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 5)
          (1UL<rowindex>, 0UL<colindex>, 3) ]

    let coo = Formats.create isArray nrows ncols (data)

    let f _i _j v =
        v |> Option.bind (fun v -> if v > 2 then Some(v * 10) else None)

    let expected =
        Formats.create isArray nrows ncols ([ (0UL<rowindex>, 1UL<colindex>, 50); (1UL<rowindex>, 0UL<colindex>, 30) ])

    let actual = Formats.cooMapi<int, int> isArray coo f

    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMapi<int, int> isArray empty input`` (isArray: bool) =
    let coo =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    let f _i _j v = v |> Option.map (fun v -> v * 2)
    let actual = Formats.cooMapi<int, int> isArray coo f

    let expected =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMapi<int, int> isArray position-dependent fill of missing cells`` (isArray: bool) =
    let nrows = 2UL<nrows>
    let ncols = 2UL<ncols>

    let data = [ (0UL<rowindex>, 0UL<colindex>, 7) ]
    let coo = Formats.create isArray nrows ncols (data)

    let f i j v =
        match v with
        | Some x -> Some x
        | None -> Some(int (uint64 i + uint64 j))

    let actual = Formats.cooMapi<int, int> isArray coo f

    let expected =
        [ (0UL<rowindex>, 0UL<colindex>, 7)
          (0UL<rowindex>, 1UL<colindex>, 1)
          (1UL<rowindex>, 0UL<colindex>, 1)
          (1UL<rowindex>, 1UL<colindex>, 2) ]

    Assert.Equal<COOEntry<int>[]>(Array.ofList expected, (Formats.toArray<int> isArray actual))

// === Formats.cooMap2i<int, int, int> isArray tests ===

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2i<int, int, int> isArray position-dependent addition`` (isArray: bool) =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 3) ]
    let d2 = [ (0UL<rowindex>, 0UL<colindex>, 10); (2UL<rowindex>, 2UL<colindex>, 30) ]

    let f i j x y =
        match x, y with
        | Some a, Some b -> Some(a + b + (int (uint64 i)))
        | Some a, None -> Some a
        | None, Some b -> Some b
        | _ -> None

    let expected =
        Formats.create isArray nrows ncols ([ (0UL<rowindex>, 0UL<colindex>, 11); (2UL<rowindex>, 2UL<colindex>, 35) ])

    let c1 = Formats.create isArray nrows ncols (d1)
    let c2 = Formats.create isArray nrows ncols (d2)
    let actual = Formats.cooMap2i<int, int, int> isArray c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2i<int, int, int> isArray mismatched positions with index`` (isArray: bool) =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 3) ]
    let d2 = [ (1UL<rowindex>, 1UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 30) ]

    let f i j x y =
        match x, y with
        | Some a, Some b -> Some(a + b)
        | Some a, None -> Some(a + (int (uint64 j)))
        | None, Some b -> Some(b + (int (uint64 i)))
        | _ -> None

    let expected =
        Formats.create
            isArray
            nrows
            ncols
            ([ (0UL<rowindex>, 0UL<colindex>, 1)
               (1UL<rowindex>, 1UL<colindex>, 11)
               (2UL<rowindex>, 2UL<colindex>, 5)
               (3UL<rowindex>, 3UL<colindex>, 33) ])

    let c1 = Formats.create isArray nrows ncols (d1)
    let c2 = Formats.create isArray nrows ncols (d2)
    let actual = Formats.cooMap2i<int, int, int> isArray c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2i<int, int, int> isArray filters None results`` (isArray: bool) =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
    let d2 = [ (0UL<rowindex>, 0UL<colindex>, 2); (2UL<rowindex>, 2UL<colindex>, 30) ]

    let f i j x y =
        match x, y with
        | Some a, Some b when a + b > 5 -> None
        | Some a, Some b -> Some(a + b)
        | _ -> None

    let expected =
        Formats.create isArray nrows ncols ([ (0UL<rowindex>, 0UL<colindex>, 3) ])

    let c1 = Formats.create isArray nrows ncols (d1)
    let c2 = Formats.create isArray nrows ncols (d2)
    let actual = Formats.cooMap2i<int, int, int> isArray c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2i<int, int, int> isArray empty inputs`` (isArray: bool) =
    let c1 =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    let c2 =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    let f _i _j x y = None
    let actual = Formats.cooMap2i<int, int, int> isArray c1 c2 f

    let expected =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    Assert.Equal(Ok expected, actual)

// === Formats.mxmcoo isArray tests ===

let private listCoo nrows ncols entries = COOList.ListCOO(nrows, ncols, entries)

let private assertBothFormats
    isArray
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (nrows1: uint64<nrows>)
    (ncols1: uint64<ncols>)
    (nrows2: uint64<nrows>)
    (ncols2: uint64<ncols>)
    (m1: (uint64<rowindex> * uint64<colindex> * 'a) list)
    (m2: (uint64<rowindex> * uint64<colindex> * 'b) list)
    (expected: (uint64<rowindex> * uint64<colindex> * 'c) list)
    =
    let arr1 = Formats.create isArray nrows1 ncols1 (m1)
    let arr2 = Formats.create isArray nrows2 ncols2 (m2)

    match Formats.mxmcoo isArray op_add op_mult arr1 arr2 with
    | Ok actual ->
        Assert.Equal(nrows1, (Formats.nrows isArray actual))
        Assert.Equal(ncols2, (Formats.ncols isArray actual))
        Assert.Equal<COOEntry<'c>[]>(Array.ofList expected, (Formats.toArray<'c> isArray actual))
    | Error e -> failwith (e.ToString())

    let lst1 = listCoo nrows1 ncols1 m1
    let lst2 = listCoo nrows2 ncols2 m2

    match COOList.mxmcoo op_add op_mult lst1 lst2 with
    | Ok actual ->
        Assert.Equal(nrows1, actual.nrows)
        Assert.Equal(ncols2, actual.ncols)
        Assert.Equal<COOEntry<'c> list>(expected, actual.entries)
        Assert.True(keysAscending actual.entries, "entries are not sorted ascending")
    | Error e -> failwith (e.ToString())


let private absorbingMult (x: int option) (y: int option) =
    match (x, y) with
    | Some a, Some b -> Some(a * b)
    | Some a, _
    | _, Some a -> Some a
    | _ -> None

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Sparse Formats.mxmcoo isArray`` (isArray: bool) =
    let m1 =
        [ 0UL<rowindex>, 0UL<colindex>, 1
          1UL<rowindex>, 1UL<colindex>, 2
          2UL<rowindex>, 2UL<colindex>, 3 ]

    let m2 =
        [ 0UL<rowindex>, 0UL<colindex>, 3
          1UL<rowindex>, 1UL<colindex>, 2
          2UL<rowindex>, 2UL<colindex>, 1 ]

    let expected =
        [ 0UL<rowindex>, 0UL<colindex>, 3
          1UL<rowindex>, 1UL<colindex>, 4
          2UL<rowindex>, 2UL<colindex>, 3 ]

    assertBothFormats isArray op_add op_mult 3UL<nrows> 3UL<ncols> 3UL<nrows> 3UL<ncols> m1 m2 expected

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Shrinking Formats.mxmcoo isArray`` (isArray: bool) =
    let m1 =
        [ 0UL<rowindex>, 0UL<colindex>, 1
          0UL<rowindex>, 2UL<colindex>, 2
          1UL<rowindex>, 1UL<colindex>, 3 ]

    let m2 =
        [ 0UL<rowindex>, 1UL<colindex>, 4
          1UL<rowindex>, 0UL<colindex>, 5
          2UL<rowindex>, 0UL<colindex>, 6 ]

    let expected =
        [ 0UL<rowindex>, 0UL<colindex>, 12
          0UL<rowindex>, 1UL<colindex>, 4
          1UL<rowindex>, 0UL<colindex>, 15 ]

    assertBothFormats isArray op_add op_mult 2UL<nrows> 3UL<ncols> 3UL<nrows> 2UL<ncols> m1 m2 expected

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.mxmcoo isArray with non-absorbing op_mult`` (isArray: bool) =
    let m1 = [ 0UL<rowindex>, 0UL<colindex>, 1; 0UL<rowindex>, 1UL<colindex>, 2 ]
    let m2 = [ 0UL<rowindex>, 0UL<colindex>, 3 ]
    let expected = [ 0UL<rowindex>, 0UL<colindex>, 5 ]
    assertBothFormats isArray op_add absorbingMult 1UL<nrows> 2UL<ncols> 2UL<nrows> 1UL<ncols> m1 m2 expected

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.mxmcoo isArray collapses products of one cell (array and list)`` (isArray: bool) =
    let m1 =
        Formats.create
            isArray
            2UL<nrows>
            2UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1)
               (0UL<rowindex>, 1UL<colindex>, 2)
               (1UL<rowindex>, 1UL<colindex>, 3) ])

    let m2 =
        Formats.create
            isArray
            2UL<nrows>
            2UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 4); (1UL<rowindex>, 0UL<colindex>, 5) ])

    let expected =
        [ (0UL<rowindex>, 0UL<colindex>, 14); (1UL<rowindex>, 0UL<colindex>, 15) ]

    let lst1 = ListCOO(3UL<nrows>, 3UL<ncols>, Formats.toList<int> isArray m1)
    let lst2 = ListCOO(3UL<nrows>, 3UL<ncols>, Formats.toList<int> isArray m2)

    match Formats.mxmcoo isArray op_add op_mult m1 m2, COOList.mxmcoo op_add op_mult lst1 lst2 with
    | Ok arr, Ok lst ->
        let arrEntries = Formats.toList<int> isArray arr
        Assert.True((expected = arrEntries), "collapses: array result differs")
        Assert.True((expected = lst.entries), "collapses: list result differs")
        Assert.True(keysAscending arrEntries)
        Assert.True(keysAscending lst.entries)
    | _ -> failwith "Formats.mxmcoo isArray failed"

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.mxmcoo isArray result stays sorted when k has multiple hits`` (isArray: bool) =
    let m1 =
        Formats.create
            isArray
            3UL<nrows>
            3UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1)
               (0UL<rowindex>, 1UL<colindex>, 2)
               (0UL<rowindex>, 2UL<colindex>, 3)
               (2UL<rowindex>, 0UL<colindex>, 7)
               (2UL<rowindex>, 2UL<colindex>, 9) ])

    let m2 =
        Formats.create
            isArray
            3UL<nrows>
            3UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 4)
               (0UL<rowindex>, 1UL<colindex>, 5)
               (1UL<rowindex>, 0UL<colindex>, 6)
               (1UL<rowindex>, 2UL<colindex>, 7)
               (2UL<rowindex>, 0UL<colindex>, 8) ])

    let lst1 = ListCOO(3UL<nrows>, 3UL<ncols>, Formats.toList<int> isArray m1)
    let lst2 = ListCOO(3UL<nrows>, 3UL<ncols>, Formats.toList<int> isArray m2)

    match Formats.mxmcoo isArray op_add op_mult m1 m2, COOList.mxmcoo op_add op_mult lst1 lst2 with
    | Ok arr, Ok lst ->
        let arrEntries = Formats.toList<int> isArray arr
        Assert.True((arrEntries = lst.entries), "array and list results differ")
        Assert.True(keysAscending arrEntries)
        Assert.True(keysAscending lst.entries)
    | _ -> failwith "Formats.mxmcoo isArray failed"

// === Formats.cooMapValues<int, int> isArray / Formats.cooMapiValues<int, int> isArray tests ===

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMapValues<int, int> isArray applies only to stored values`` (isArray: bool) =
    let coo =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let actual = Formats.cooMapValues<int, int> isArray coo (fun v -> Some(v * 10))

    Assert.Equal(2, (Formats.toArray<int> isArray actual).Length)

    Assert.Equal(
        Array.tryFind (fun (i, j, _) -> i = 0UL<rowindex> && j = 0UL<colindex>) (Formats.toArray<int> isArray actual),
        Some(0UL<rowindex>, 0UL<colindex>, 10)
    )

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMapiValues<int, int> isArray applies indexed only to stored values`` (isArray: bool) =
    let coo =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([ (1UL<rowindex>, 2UL<colindex>, 5) ])

    let actual =
        Formats.cooMapiValues<int, int> isArray coo (fun i j v -> Some(v + int (uint64 i) + int (uint64 j)))

    Assert.Equal(1, (Formats.toArray<int> isArray actual).Length)

    Assert.Equal(
        Array.tryFind (fun (i, j, _) -> i = 1UL<rowindex> && j = 2UL<colindex>) (Formats.toArray<int> isArray actual),
        Some(1UL<rowindex>, 2UL<colindex>, 8)
    )

// === Formats.cooMap2<int, int, int> isArray variants tests ===

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2Values<int, int, int> isArray applies only where both present`` (isArray: bool) =
    let c1 =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([ (0UL<rowindex>, 0UL<colindex>, 10) ])

    match Formats.cooMap2Values<int, int, int> isArray c1 c2 (fun a b -> Some(a + b)) with
    | Ok actual ->
        Assert.Equal(1, (Formats.toArray<int> isArray actual).Length)

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 0UL<rowindex> && j = 0UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(0UL<rowindex>, 0UL<colindex>, 11)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2AllCells<int, int, int> isArray equals Formats.cooMap2<int, int, int> isArray`` (isArray: bool) =
    let c1 =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([ (0UL<rowindex>, 0UL<colindex>, 10) ])

    let f a b =
        match a, b with
        | Some x, Some y -> Some(x + y)
        | _ -> None

    Assert.Equal(Formats.cooMap2<int, int, int> isArray c1 c2 f, Formats.cooMap2AllCells<int, int, int> isArray c1 c2 f)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2AtLeastOne<int, int, int> isArray distinguishes both left right`` (isArray: bool) =
    let c1 =
        Formats.create
            isArray
            3UL<nrows>
            3UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        Formats.create
            isArray
            3UL<nrows>
            3UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 10); (2UL<rowindex>, 2UL<colindex>, 30) ])

    let f =
        function
        | AtLeastOne.Both(a, b) -> Some(a + b)
        | AtLeastOne.Left a -> Some(a * 100)
        | AtLeastOne.Right b -> Some(b * -1)

    match Formats.cooMap2AtLeastOne<int, int, int> isArray c1 c2 f with
    | Ok actual ->
        Assert.Equal(3, (Formats.toArray<int> isArray actual).Length)

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 0UL<rowindex> && j = 0UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(0UL<rowindex>, 0UL<colindex>, 11)
        )

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 1UL<rowindex> && j = 1UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(1UL<rowindex>, 1UL<colindex>, 200)
        )

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 2UL<rowindex> && j = 2UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(2UL<rowindex>, 2UL<colindex>, -30)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2LeftValues<int, int, int> isArray applies where left present`` (isArray: bool) =
    let c1 =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([ (0UL<rowindex>, 0UL<colindex>, 10) ])

    match Formats.cooMap2LeftValues<int, int, int> isArray c1 c2 (fun a b -> Some(a + (defaultArg b 0))) with
    | Ok actual ->
        Assert.Equal(2, (Formats.toArray<int> isArray actual).Length)

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 0UL<rowindex> && j = 0UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(0UL<rowindex>, 0UL<colindex>, 11)
        )

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 1UL<rowindex> && j = 1UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(1UL<rowindex>, 1UL<colindex>, 2)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2<int, int, int> isArray sizes mismatch`` (isArray: bool) =
    let c1 =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    let c2 =
        Formats.create isArray 2UL<nrows> 2UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    let f a b = None

    Assert.Equal(Error Error.InconsistentSizeOfArguments, Formats.cooMap2<int, int, int> isArray c1 c2 f)

// === Formats.cooMap2i<int, int, int> isArray variants tests ===

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2iValues<int, int, int> isArray applies indexed where both present`` (isArray: bool) =
    let c1 =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([ (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (1UL<rowindex>, 1UL<colindex>, 10); (2UL<rowindex>, 2UL<colindex>, 20) ])

    let f i j a b =
        Some(a + b + int (uint64 i) + int (uint64 j))

    match Formats.cooMap2iValues<int, int, int> isArray c1 c2 f with
    | Ok actual ->
        Assert.Equal(1, (Formats.toArray<int> isArray actual).Length)

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 1UL<rowindex> && j = 1UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(1UL<rowindex>, 1UL<colindex>, 14)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2iAllCells<int, int, int> isArray equals Formats.cooMap2i<int, int, int> isArray`` (isArray: bool) =
    let c1 =
        Formats.create
            isArray
            4UL<nrows>
            4UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([ (0UL<rowindex>, 0UL<colindex>, 10) ])

    let f i j a b =
        match a, b with
        | Some x, Some y -> Some(x + y + int (uint64 i))
        | _ -> None

    Assert.Equal(
        Formats.cooMap2i<int, int, int> isArray c1 c2 f,
        Formats.cooMap2iAllCells<int, int, int> isArray c1 c2 f
    )

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2iAtLeastOne<int, int, int> isArray passes indices and side`` (isArray: bool) =
    let c1 =
        Formats.create isArray 2UL<nrows> 2UL<ncols> ([ (0UL<rowindex>, 0UL<colindex>, 1) ])

    let c2 =
        Formats.create
            isArray
            2UL<nrows>
            2UL<ncols>
            ([ (0UL<rowindex>, 0UL<colindex>, 10); (1UL<rowindex>, 1UL<colindex>, 20) ])

    let f i j =
        function
        | AtLeastOne.Both(a, b) -> Some(a + b + int (uint64 i) + int (uint64 j))
        | AtLeastOne.Left a -> Some(a)
        | AtLeastOne.Right b -> Some(b + int (uint64 i) * 100 + int (uint64 j))

    match Formats.cooMap2iAtLeastOne<int, int, int> isArray c1 c2 f with
    | Ok actual ->
        Assert.Equal(2, (Formats.toArray<int> isArray actual).Length)

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 0UL<rowindex> && j = 0UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(0UL<rowindex>, 0UL<colindex>, 11)
        )

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 1UL<rowindex> && j = 1UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(1UL<rowindex>, 1UL<colindex>, 121)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2iLeftValues<int, int, int> isArray applies indexed where left present`` (isArray: bool) =
    let c1 =
        Formats.create isArray 2UL<nrows> 2UL<ncols> ([ (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        Formats.create isArray 2UL<nrows> 2UL<ncols> ([ (1UL<rowindex>, 1UL<colindex>, 10) ])

    let f i j a b =
        Some(a + (defaultArg b 0) + int (uint64 i) * 10 + int (uint64 j))

    match Formats.cooMap2iLeftValues<int, int, int> isArray c1 c2 f with
    | Ok actual ->
        Assert.Equal(1, (Formats.toArray<int> isArray actual).Length)

        Assert.Equal(
            Array.tryFind
                (fun (i, j, _) -> i = 1UL<rowindex> && j = 1UL<colindex>)
                (Formats.toArray<int> isArray actual),
            Some(1UL<rowindex>, 1UL<colindex>, 23)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Formats.cooMap2i<int, int, int> isArray sizes mismatch`` (isArray: bool) =
    let c1 =
        Formats.create isArray 4UL<nrows> 4UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    let c2 =
        Formats.create isArray 2UL<nrows> 2UL<ncols> ([]: (uint64<rowindex> * uint64<colindex> * int) list)

    let f _i _j a b = None

    Assert.Equal(Error Error.InconsistentSizeOfArguments, Formats.cooMap2i<int, int, int> isArray c1 c2 f)
