module COO.Tests

open System
open Xunit

open Matrix
open COO
open Common

let op_add x y =
    match (x, y) with
    | Some(a), Some(b) -> Some(a + b)
    | Some a, None
    | None, Some a -> Some a
    | _ -> None

let op_mult x y =
    match (x, y) with
    | Some(a), Some(b) -> Some(a * b)
    | _ -> None

// === cooGet tests ===

[<Fact>]
let ``cooGet existing value`` () =
    let coo =
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3) ]
        )

    let actual = cooGet (coo, 0UL<rowindex>, 1UL<colindex>)

    Assert.Equal(Ok(Some 2), actual)

[<Fact>]
let ``cooGet missing value`` () =
    let coo =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let actual = cooGet (coo, 2UL<rowindex>, 2UL<colindex>)

    Assert.Equal(Ok None, actual)

[<Fact>]
let ``cooGet out of bounds`` () =
    let coo =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    Assert.Throws<System.ArgumentOutOfRangeException>(fun () ->
        cooGet (coo, 5UL<rowindex>, 5UL<colindex>) |> ignore)

// === cooUpdate tests ===

[<Fact>]
let ``cooUpdate replaces existing`` () =
    let coo =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let expected =
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 99); (1UL<rowindex>, 1UL<colindex>, 2) ]
        )

    let actual = cooUpdate (coo, 0UL<rowindex>, 0UL<colindex>, 99)

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooUpdate inserts new in middle`` () =
    let coo =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 2) ])

    let expected =
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (1UL<rowindex>, 1UL<colindex>, 10)
              (2UL<rowindex>, 2UL<colindex>, 2) ]
        )

    let actual = cooUpdate (coo, 1UL<rowindex>, 1UL<colindex>, 10)

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooUpdate inserts at end`` () =
    let coo =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    let expected =
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1); (3UL<rowindex>, 3UL<colindex>, 20) ]
        )

    let actual = cooUpdate (coo, 3UL<rowindex>, 3UL<colindex>, 20)

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooUpdate out of bounds`` () =
    let coo =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    Assert.Throws<System.ArgumentOutOfRangeException>(fun () ->
        cooUpdate (coo, 5UL<rowindex>, 5UL<colindex>, 99) |> ignore)

// === cooMap tests ===

[<Fact>]
let ``cooMap doubles values`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 2)
          (1UL<rowindex>, 0UL<colindex>, 3)
          (1UL<rowindex>, 1UL<colindex>, 4) ]
        |> List.sort

    let coo = CoordinateList(nrows, ncols, data)

    let f v = v |> Option.map (fun v -> v * 2)

    let expected =
        CoordinateList(
            nrows,
            ncols,
            [ (0UL<rowindex>, 0UL<colindex>, 2)
              (0UL<rowindex>, 1UL<colindex>, 4)
              (1UL<rowindex>, 0UL<colindex>, 6)
              (1UL<rowindex>, 1UL<colindex>, 8) ]
        )

    let actual = cooMap coo f

    Assert.Equal(expected, actual)

[<Fact>]
let ``cooMap filters None results`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 2)
          (1UL<rowindex>, 0UL<colindex>, 3)
          (1UL<rowindex>, 1UL<colindex>, 4) ]

    let coo = CoordinateList(nrows, ncols, data)

    let f v =
        v
        |> Option.bind (fun v ->
            match v with
            | 1 -> None
            | _ -> Some(v * 10))

    let expected =
        CoordinateList(
            nrows,
            ncols,
            [ (0UL<rowindex>, 1UL<colindex>, 20)
              (1UL<rowindex>, 0UL<colindex>, 30)
              (1UL<rowindex>, 1UL<colindex>, 40) ]
        )

    let actual = cooMap coo f

    Assert.Equal(expected, actual)

[<Fact>]
let ``cooMap fills missing cells (general form)`` () =
    let nrows = 3UL<nrows>
    let ncols = 3UL<ncols>

    let data = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 5) ]

    let coo = CoordinateList(nrows, ncols, data)

    let f v = Some(defaultArg v 0)

    let actual = cooMap coo f

    Assert.Equal(nrows, actual.nrows)
    Assert.Equal(ncols, actual.ncols)
    Assert.Equal(9, actual.list.Length)

    Assert.Equal(
        List.tryFind (fun (i, j, _) -> i = 2UL<rowindex> && j = 2UL<colindex>) actual.list,
        Some(2UL<rowindex>, 2UL<colindex>, 5)
    )

[<Fact>]
let ``cooMap zero-size matrix`` () =
    let coo = CoordinateList(0UL<nrows>, 0UL<ncols>, [])
    let f v = v |> Option.map (fun v -> v * 2)
    let actual = cooMap coo f
    let expected = CoordinateList(0UL<nrows>, 0UL<ncols>, [])
    Assert.Equal(expected, actual)

// === cooMap2 tests ===

[<Fact>]
let ``cooMap2 addition`` () =
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
        CoordinateList(
            nrows,
            ncols,
            [ (0UL<rowindex>, 3UL<colindex>, 10)
              (3UL<rowindex>, 3UL<colindex>, 33)
              (9UL<rowindex>, 2UL<colindex>, 5)
              (3UL<rowindex>, 11UL<colindex>, 1) ]
            |> List.sort
        )

    let c1 = CoordinateList(nrows, ncols, d1)
    let c2 = CoordinateList(nrows, ncols, d2)

    let actual = cooMap2 c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooMap2 with mismatched positions`` () =
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
        CoordinateList(
            nrows,
            ncols,
            [ (0UL<rowindex>, 0UL<colindex>, 101)
              (1UL<rowindex>, 1UL<colindex>, 210)
              (2UL<rowindex>, 2UL<colindex>, 103)
              (3UL<rowindex>, 3UL<colindex>, 230) ]
        )

    let c1 = CoordinateList(nrows, ncols, d1)
    let c2 = CoordinateList(nrows, ncols, d2)

    let actual = cooMap2 c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooMap2 dense filters None from existing entries`` () =
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
        CoordinateList(
            nrows,
            ncols,
            [ (0UL<rowindex>, 1UL<colindex>, 0)
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
              (3UL<rowindex>, 3UL<colindex>, 0) ]
        )

    let c1 = CoordinateList(nrows, ncols, d1)
    let c2 = CoordinateList(nrows, ncols, d2)

    let actual = cooMap2 c1 c2 f

    Assert.Equal(Ok expected, actual)

// === cooMapi tests ===

[<Fact>]
let ``cooMapi position-dependent values`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (1UL<rowindex>, 1UL<colindex>, 2)
          (2UL<rowindex>, 3UL<colindex>, 3) ]
        |> List.sort

    let coo = CoordinateList(nrows, ncols, data)

    let f i j v =
        v |> Option.map (fun v -> v + (int (uint64 i)))

    let expected =
        CoordinateList(
            nrows,
            ncols,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (1UL<rowindex>, 1UL<colindex>, 3)
              (2UL<rowindex>, 3UL<colindex>, 5) ]
        )

    let actual = cooMapi coo f

    Assert.Equal(expected, actual)

[<Fact>]
let ``cooMapi filters None results`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 5)
          (1UL<rowindex>, 0UL<colindex>, 3) ]

    let coo = CoordinateList(nrows, ncols, data)

    let f _i _j v =
        v |> Option.bind (fun v -> if v > 2 then Some(v * 10) else None)

    let expected =
        CoordinateList(nrows, ncols, [ (0UL<rowindex>, 1UL<colindex>, 50); (1UL<rowindex>, 0UL<colindex>, 30) ])

    let actual = cooMapi coo f

    Assert.Equal(expected, actual)

[<Fact>]
let ``cooMapi empty input`` () =
    let coo = CoordinateList(4UL<nrows>, 4UL<ncols>, [])
    let f _i _j v = v |> Option.map (fun v -> v * 2)
    let actual = cooMapi coo f
    let expected = CoordinateList(4UL<nrows>, 4UL<ncols>, [])
    Assert.Equal(expected, actual)

[<Fact>]
let ``cooMapi fills missing cells (general form)`` () =
    let nrows = 3UL<nrows>
    let ncols = 3UL<ncols>

    let data = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 5) ]
    let coo = CoordinateList(nrows, ncols, data)

    let f _i _j v = Some(defaultArg v 0)

    let actual = cooMapi coo f

    Assert.Equal(nrows, actual.nrows)
    Assert.Equal(ncols, actual.ncols)
    Assert.Equal(9, actual.list.Length)

    Assert.Equal(
        List.tryFind (fun (i, j, _) -> i = 2UL<rowindex> && j = 2UL<colindex>) actual.list,
        Some(2UL<rowindex>, 2UL<colindex>, 5)
    )

[<Fact>]
let ``cooMapi position-dependent fill of missing cells`` () =
    let nrows = 2UL<nrows>
    let ncols = 2UL<ncols>

    let data = [ (0UL<rowindex>, 0UL<colindex>, 7) ]
    let coo = CoordinateList(nrows, ncols, data)

    let f i j v =
        match v with
        | Some x -> Some x
        | None -> Some(int (uint64 i + uint64 j))

    let actual = cooMapi coo f

    let expected =
        [ (0UL<rowindex>, 0UL<colindex>, 7)
          (0UL<rowindex>, 1UL<colindex>, 1)
          (1UL<rowindex>, 0UL<colindex>, 1)
          (1UL<rowindex>, 1UL<colindex>, 2) ]

    Assert.Equal<list<uint64<rowindex> * uint64<colindex> * int>>(expected, actual.list)

[<Fact>]
let ``cooMapi zero-size matrix`` () =
    let coo = CoordinateList(0UL<nrows>, 0UL<ncols>, [])
    let f _i _j v = v |> Option.map (fun v -> v * 2)
    let actual = cooMapi coo f
    let expected = CoordinateList(0UL<nrows>, 0UL<ncols>, [])
    Assert.Equal(expected, actual)

// === cooMap2i tests ===

[<Fact>]
let ``cooMap2i position-dependent addition`` () =
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
        CoordinateList(nrows, ncols, [ (0UL<rowindex>, 0UL<colindex>, 11); (2UL<rowindex>, 2UL<colindex>, 35) ])

    let c1 = CoordinateList(nrows, ncols, d1)
    let c2 = CoordinateList(nrows, ncols, d2)
    let actual = cooMap2i c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooMap2i mismatched positions with index`` () =
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
        CoordinateList(
            nrows,
            ncols,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (2UL<rowindex>, 2UL<colindex>, 5)
              (3UL<rowindex>, 3UL<colindex>, 33) ]
        )

    let c1 = CoordinateList(nrows, ncols, d1)
    let c2 = CoordinateList(nrows, ncols, d2)
    let actual = cooMap2i c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooMap2i filters None results`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
    let d2 = [ (0UL<rowindex>, 0UL<colindex>, 2); (2UL<rowindex>, 2UL<colindex>, 30) ]

    let f i j x y =
        match x, y with
        | Some a, Some b when a + b > 5 -> None
        | Some a, Some b -> Some(a + b)
        | _ -> None

    let expected = CoordinateList(nrows, ncols, [ (0UL<rowindex>, 0UL<colindex>, 3) ])

    let c1 = CoordinateList(nrows, ncols, d1)
    let c2 = CoordinateList(nrows, ncols, d2)
    let actual = cooMap2i c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooMap2i empty inputs`` () =
    let c1 = CoordinateList(4UL<nrows>, 4UL<ncols>, [])
    let c2 = CoordinateList(4UL<nrows>, 4UL<ncols>, [])
    let f _i _j x y = None
    let actual = cooMap2i c1 c2 f
    let expected = CoordinateList(4UL<nrows>, 4UL<ncols>, [])
    Assert.Equal(Ok expected, actual)

// === mxmcoo tests ===

[<Fact>]
let ``Sparse mxmcoo`` () =
    let m1 =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 1
              1UL<rowindex>, 1UL<colindex>, 2
              2UL<rowindex>, 2UL<colindex>, 3 ]

        CoordinateList(3UL<nrows>, 3UL<ncols>, d)

    let m2 =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 3
              1UL<rowindex>, 1UL<colindex>, 2
              2UL<rowindex>, 2UL<colindex>, 1 ]

        CoordinateList(3UL<nrows>, 3UL<ncols>, d)

    let expected =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 3
              1UL<rowindex>, 1UL<colindex>, 4
              2UL<rowindex>, 2UL<colindex>, 3 ]

        CoordinateList(3UL<nrows>, 3UL<ncols>, d)

    match COO.mxmcoo op_add op_mult m1 m2 with
    | Ok actual ->
        Assert.Equal(expected.nrows, actual.nrows)
        Assert.Equal(expected.ncols, actual.ncols)
        Assert.Equal<List<_>>(expected.list, actual.list)
    | Error e -> failwith (e.ToString())

[<Fact>]
let ``Shrinking mxmcoo`` () =
    let m1 =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 1
              0UL<rowindex>, 2UL<colindex>, 2
              1UL<rowindex>, 1UL<colindex>, 3 ]

        CoordinateList(2UL<nrows>, 3UL<ncols>, d)

    let m2 =
        let d =
            [ 0UL<rowindex>, 1UL<colindex>, 4
              1UL<rowindex>, 0UL<colindex>, 5
              2UL<rowindex>, 0UL<colindex>, 6 ]

        CoordinateList(3UL<nrows>, 2UL<ncols>, d)

    let expected =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 12
              0UL<rowindex>, 1UL<colindex>, 4
              1UL<rowindex>, 0UL<colindex>, 15 ]

        CoordinateList(2UL<nrows>, 2UL<ncols>, d)

    match COO.mxmcoo op_add op_mult m1 m2 with
    | Ok actual ->
        Assert.Equal(expected.nrows, actual.nrows)
        Assert.Equal(expected.ncols, actual.ncols)
        Assert.Equal<List<_>>(expected.list, actual.list)
    | Error e -> failwith (e.ToString())


[<Fact>]
let ``mxmcoo with non-absorbing op_mult`` () =
    let op_add x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | Some a, _
        | _, Some a -> Some a
        | _ -> None

    let op_mult x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a * b)
        | Some a, _
        | _, Some a -> Some a
        | _ -> None

    let m1 =
        let d = [ 0UL<rowindex>, 0UL<colindex>, 1; 0UL<rowindex>, 1UL<colindex>, 2 ]

        CoordinateList(1UL<nrows>, 2UL<ncols>, d)

    let m2 =
        let d = [ 0UL<rowindex>, 0UL<colindex>, 3 ]

        CoordinateList(2UL<nrows>, 1UL<ncols>, d)

    match COO.mxmcoo op_add op_mult m1 m2 with
    | Ok actual ->
        Assert.Equal(1UL<nrows>, actual.nrows)
        Assert.Equal(1UL<ncols>, actual.ncols)
        Assert.Equal(1, actual.list.Length)
        Assert.Equal(Some 5, actual.list |> List.tryHead |> Option.map (fun (_, _, v) -> v))
    | Error e -> failwith (e.ToString())

// === cooMapValues / cooMapiValues tests ===

[<Fact>]
let ``cooMapValues applies only to stored values`` () =
    let coo =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let actual = cooMapValues coo (fun v -> Some(v * 10))

    Assert.Equal(2, actual.list.Length)

    Assert.Equal(
        List.tryFind (fun (i, j, _) -> i = 0UL<rowindex> && j = 0UL<colindex>) actual.list,
        Some(0UL<rowindex>, 0UL<colindex>, 10)
    )

[<Fact>]
let ``cooMapiValues applies indexed only to stored values`` () =
    let coo =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (1UL<rowindex>, 2UL<colindex>, 5) ])

    let actual =
        cooMapiValues coo (fun i j v -> Some(v + int (uint64 i) + int (uint64 j)))

    Assert.Equal(1, actual.list.Length)

    Assert.Equal(
        List.tryFind (fun (i, j, _) -> i = 1UL<rowindex> && j = 2UL<colindex>) actual.list,
        Some(1UL<rowindex>, 2UL<colindex>, 8)
    )

// === cooMap2 variants tests ===

[<Fact>]
let ``cooMap2Values applies only where both present`` () =
    let c1 =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 10) ])

    match cooMap2Values c1 c2 (fun a b -> Some(a + b)) with
    | Ok actual ->
        Assert.Equal(1, actual.list.Length)

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 0UL<rowindex> && j = 0UL<colindex>) actual.list,
            Some(0UL<rowindex>, 0UL<colindex>, 11)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Fact>]
let ``cooMap2AllCells equals cooMap2`` () =
    let c1 =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 10) ])

    let f a b =
        match a, b with
        | Some x, Some y -> Some(x + y)
        | _ -> None

    Assert.Equal(cooMap2 c1 c2 f, cooMap2AllCells c1 c2 f)

[<Fact>]
let ``cooMap2AtLeastOne distinguishes both left right`` () =
    let c1 =
        CoordinateList(3UL<nrows>, 3UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 10); (2UL<rowindex>, 2UL<colindex>, 30) ]
        )

    let f =
        function
        | AtLeastOne.Both(a, b) -> Some(a + b)
        | AtLeastOne.Left a -> Some(a * 100)
        | AtLeastOne.Right b -> Some(b * -1)

    match cooMap2AtLeastOne c1 c2 f with
    | Ok actual ->
        Assert.Equal(3, actual.list.Length)

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 0UL<rowindex> && j = 0UL<colindex>) actual.list,
            Some(0UL<rowindex>, 0UL<colindex>, 11)
        )

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 1UL<rowindex> && j = 1UL<colindex>) actual.list,
            Some(1UL<rowindex>, 1UL<colindex>, 200)
        )

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 2UL<rowindex> && j = 2UL<colindex>) actual.list,
            Some(2UL<rowindex>, 2UL<colindex>, -30)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Fact>]
let ``cooMap2LeftValues applies where left present`` () =
    let c1 =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 10) ])

    match cooMap2LeftValues c1 c2 (fun a b -> Some(a + (defaultArg b 0))) with
    | Ok actual ->
        Assert.Equal(2, actual.list.Length)

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 0UL<rowindex> && j = 0UL<colindex>) actual.list,
            Some(0UL<rowindex>, 0UL<colindex>, 11)
        )

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 1UL<rowindex> && j = 1UL<colindex>) actual.list,
            Some(1UL<rowindex>, 1UL<colindex>, 2)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Fact>]
let ``cooMap2 sizes mismatch`` () =
    let c1 = CoordinateList(4UL<nrows>, 4UL<ncols>, [])
    let c2 = CoordinateList(2UL<nrows>, 2UL<ncols>, [])
    let f a b = None

    Assert.Equal(Error Error.InconsistentSizeOfArguments, cooMap2 c1 c2 f)

// === cooMap2i variants tests ===

[<Fact>]
let ``cooMap2iValues applies indexed where both present`` () =
    let c1 =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 10); (2UL<rowindex>, 2UL<colindex>, 20) ]
        )

    let f i j a b =
        Some(a + b + int (uint64 i) + int (uint64 j))

    match cooMap2iValues c1 c2 f with
    | Ok actual ->
        Assert.Equal(1, actual.list.Length)

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 1UL<rowindex> && j = 1UL<colindex>) actual.list,
            Some(1UL<rowindex>, 1UL<colindex>, 14)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Fact>]
let ``cooMap2iAllCells equals cooMap2i`` () =
    let c1 =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 10) ])

    let f i j a b =
        match a, b with
        | Some x, Some y -> Some(x + y + int (uint64 i))
        | _ -> None

    Assert.Equal(cooMap2i c1 c2 f, cooMap2iAllCells c1 c2 f)

[<Fact>]
let ``cooMap2iAtLeastOne passes indices and side`` () =
    let c1 =
        CoordinateList(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    let c2 =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 10); (1UL<rowindex>, 1UL<colindex>, 20) ]
        )

    let f i j =
        function
        | AtLeastOne.Both(a, b) -> Some(a + b + int (uint64 i) + int (uint64 j))
        | AtLeastOne.Left a -> Some(a)
        | AtLeastOne.Right b -> Some(b + int (uint64 i) * 100 + int (uint64 j))

    match cooMap2iAtLeastOne c1 c2 f with
    | Ok actual ->
        Assert.Equal(2, actual.list.Length)

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 0UL<rowindex> && j = 0UL<colindex>) actual.list,
            Some(0UL<rowindex>, 0UL<colindex>, 11)
        )

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 1UL<rowindex> && j = 1UL<colindex>) actual.list,
            Some(1UL<rowindex>, 1UL<colindex>, 121)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Fact>]
let ``cooMap2iLeftValues applies indexed where left present`` () =
    let c1 =
        CoordinateList(2UL<nrows>, 2UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 2) ])

    let c2 =
        CoordinateList(2UL<nrows>, 2UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 10) ])

    let f i j a b =
        Some(a + (defaultArg b 0) + int (uint64 i) * 10 + int (uint64 j))

    match cooMap2iLeftValues c1 c2 f with
    | Ok actual ->
        Assert.Equal(1, actual.list.Length)

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 1UL<rowindex> && j = 1UL<colindex>) actual.list,
            Some(1UL<rowindex>, 1UL<colindex>, 23)
        )
    | Error e -> failwithf "unexpected error %A" e

[<Fact>]
let ``cooMap2i sizes mismatch`` () =
    let c1 = CoordinateList(4UL<nrows>, 4UL<ncols>, [])
    let c2 = CoordinateList(2UL<nrows>, 2UL<ncols>, [])
    let f _i _j a b = None

    Assert.Equal(Error Error.InconsistentSizeOfArguments, cooMap2i c1 c2 f)
