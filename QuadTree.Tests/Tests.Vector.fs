module Vector.Tests

open Xunit
open System

open Vector
open Common

let private fromCoordinateListUnchecked (lst: Vector.CoordinateList<'a>) =
    match Vector.fromCoordinateList lst with
    | Ok v -> v
    | Error e -> failwith e

let printVector (vector: SparseVector<_>) =
    printfn "Vector:"
    printfn "   Length: %A" vector.length
    printfn "   Nvals: %A" vector.nvals
    printfn "   Storage:"
    printfn "      Size: %A" vector.storage.size
    printfn "      Data: %A" vector.storage.data

[<Fact>]
let ``Simple Vector.map. Length is power of two.`` () =
    let v =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Leaf(UserValue(Some(2)))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(8UL<dataLength>, 6UL<nvals>, store)

    let f x =
        match x with
        | Some(a) -> Some(a * 2)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Leaf(UserValue(Some(4)))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(8UL<dataLength>, 6UL<nvals>, store)

    let actual = Vector.map v f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.map. Length is not power of two.`` () =
    let v =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(Dummy))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(6UL<dataLength>, 2UL<nvals>, store)

    let f x =
        match x with
        | Some(a) -> Some(a * 2)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(Dummy))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(6UL<dataLength>, 2UL<nvals>, store)

    let actual = Vector.map v f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.mapi. Length is power of two, multiply by index.`` () =
    let dataLength = 8UL<dataLength>

    let d =
        [ (0UL<index>, 1)
          (1UL<index>, 1)
          (2UL<index>, 1)
          (3UL<index>, 1)
          (4UL<index>, 2)
          (5UL<index>, 2)
          (6UL<index>, 2)
          (7UL<index>, 2) ]

    match Vector.fromCoordinateList (Vector.CoordinateList(dataLength, d)) with
    | Result.Error _ -> Assert.Fail()
    | Result.Ok v ->
        let f (idx: uint64<index>) x =
            match x with
            | Some(a) -> Some(a * int idx)
            | _ -> None

        let expectedResult =
            let expectedData =
                [ (0UL<index>, 0)
                  (1UL<index>, 1)
                  (2UL<index>, 2)
                  (3UL<index>, 3)
                  (4UL<index>, 8)
                  (5UL<index>, 10)
                  (6UL<index>, 12)
                  (7UL<index>, 14) ]

            Vector.fromCoordinateList (Vector.CoordinateList(dataLength, expectedData))

        let actual = Vector.mapi v f
        Assert.Equal(expectedResult, Ok actual)

[<Fact>]
let ``Simple Vector.mapi. Length is not power of two.`` () =
    // Build vector [1, 1, 1, 1, 1, 1] with dummy at end
    let dataLength = 6UL<dataLength>

    let d =
        [ (0UL<index>, 1)
          (1UL<index>, 1)
          (2UL<index>, 1)
          (3UL<index>, 1)
          (4UL<index>, 1)
          (5UL<index>, 1) ]

    match Vector.fromCoordinateList (Vector.CoordinateList(dataLength, d)) with
    | Result.Error _ -> Assert.Fail()
    | Result.Ok v ->
        // f idx x = x * idx
        let f (idx: uint64<index>) x =
            match x with
            | Some(a) -> Some(a * int idx)
            | _ -> None

        // Expected: [0, 1, 2, 3, 4, 5] (1*idx for each position)
        let expectedResult =
            let expectedData =
                [ (0UL<index>, 0)
                  (1UL<index>, 1)
                  (2UL<index>, 2)
                  (3UL<index>, 3)
                  (4UL<index>, 4)
                  (5UL<index>, 5) ]

            Vector.fromCoordinateList (Vector.CoordinateList(dataLength, expectedData))

        let actual = Vector.mapi v f
        Assert.Equal(expectedResult, Ok actual)

[<Fact>]
let ``Simple Vector.mapi. Uniform leaf expansion.`` () =
    // Vector of length 1 with value 5
    let v =
        let tree = Vector.btree.Leaf(UserValue(Some(5)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let f (idx: uint64<index>) x =
        match x with
        | Some(a) -> Some(a + int idx)
        | _ -> None

    let expected =
        let tree = Vector.btree.Leaf(UserValue(Some(5)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let actual = Vector.mapi v f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.mapi. All indices identity.`` () =
    // Vector with values matching their indices
    let dataLength = 4UL<dataLength>
    let d = [ (0UL<index>, 0); (2UL<index>, 2) ]

    match Vector.fromCoordinateList (Vector.CoordinateList(dataLength, d)) with
    | Result.Error _ -> Assert.Fail()
    | Result.Ok v ->
        let f (idx: uint64<index>) x =
            match x with
            | Some(a) when a = int idx -> Some a
            | _ -> None

        let expectedResult =
            Vector.fromCoordinateList (Vector.CoordinateList(dataLength, d))

        let actual = Vector.mapi v f
        Assert.Equal(expectedResult, Ok actual)

[<Fact>]
let ``Simple Vector.map2. Length is power of two.`` () =
    let v1 =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Leaf(UserValue(Some(2)))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(8UL<dataLength>, 6UL<nvals>, store)

    let v2 =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(UserValue(Some(1))))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(8UL<dataLength>, 4UL<nvals>, store)

    let f x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(3))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(UserValue(Some(3))))
            )

        let store = Storage(8UL<storageSize>, tree)
        Ok(SparseVector(8UL<dataLength>, 4UL<nvals>, store))

    let actual = Vector.map2 v1 v2 f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.map2. Length is not power of two.`` () =
    let v1 =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(Dummy))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(6UL<dataLength>, 2UL<nvals>, store)

    let v2 =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(Dummy))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(6UL<dataLength>, 2UL<nvals>, store)

    let f x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(3))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(Dummy))
            )

        let store = Storage(8UL<storageSize>, tree)
        Ok(SparseVector(6UL<dataLength>, 2UL<nvals>, store))

    let actual = Vector.map2 v1 v2 f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.map2i. Length is power of two.`` () =
    let dataLength = 4UL<dataLength>
    let d1 = [ (0UL<index>, 1); (1UL<index>, 2); (2UL<index>, 3); (3UL<index>, 4) ]
    let d2 = [ (0UL<index>, 10); (1UL<index>, 20); (2UL<index>, 30); (3UL<index>, 40) ]

    match
        Vector.fromCoordinateList (Vector.CoordinateList(dataLength, d1)),
        Vector.fromCoordinateList (Vector.CoordinateList(dataLength, d2))
    with
    | Result.Ok v1, Result.Ok v2 ->
        let f idx x y =
            match (x, y) with
            | Some(a), Some(b) -> Some(a + b + int idx)
            | _ -> None

        let actualResult = Vector.map2i v1 v2 f

        let expectedResult =
            Vector.fromCoordinateList (
                Vector.CoordinateList(
                    dataLength,
                    [ (0UL<index>, 11); (1UL<index>, 23); (2UL<index>, 35); (3UL<index>, 47) ]
                )
            )

        match actualResult, expectedResult with
        | Result.Ok actual, Result.Ok expected -> Assert.Equal(expected, actual)
        | _ -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple Vector.map2i. Length is not power of two.`` () =
    let dataLength = 6UL<dataLength>

    let d1 =
        [ (0UL<index>, 1)
          (1UL<index>, 2)
          (2UL<index>, 3)
          (3UL<index>, 4)
          (4UL<index>, 5)
          (5UL<index>, 6) ]

    let d2 =
        [ (0UL<index>, 10)
          (1UL<index>, 10)
          (2UL<index>, 10)
          (3UL<index>, 10)
          (4UL<index>, 10)
          (5UL<index>, 10) ]

    match
        Vector.fromCoordinateList (Vector.CoordinateList(dataLength, d1)),
        Vector.fromCoordinateList (Vector.CoordinateList(dataLength, d2))
    with
    | Result.Ok v1, Result.Ok v2 ->
        let f idx x y =
            match (x, y) with
            | Some(a), Some(b) -> Some(a * int idx + b)
            | _ -> None

        let actualResult = Vector.map2i v1 v2 f

        let expectedResult =
            Vector.fromCoordinateList (
                Vector.CoordinateList(
                    dataLength,
                    [ (0UL<index>, 10)
                      (1UL<index>, 12)
                      (2UL<index>, 16)
                      (3UL<index>, 22)
                      (4UL<index>, 30)
                      (5UL<index>, 40) ]
                )
            )

        match actualResult, expectedResult with
        | Result.Ok actual, Result.Ok expected -> Assert.Equal(expected, actual)
        | _ -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple Vector.map2i. Mixed values.`` () =
    let d1 = [ (0UL<index>, 1); (2UL<index>, 3) ]
    let d2 = [ (1UL<index>, 10); (3UL<index>, 30) ]
    let dataLength = 4UL<dataLength>

    match
        Vector.fromCoordinateList (Vector.CoordinateList(dataLength, d1)),
        Vector.fromCoordinateList (Vector.CoordinateList(dataLength, d2))
    with
    | Result.Ok v1, Result.Ok v2 ->
        let f idx x y =
            match (x, y) with
            | Some(a), Some(b) -> Some(a + b)
            | Some(a), None -> Some(int idx + a * 2)
            | None, Some(b) -> Some(int idx * b * 3)
            | _ -> None

        let actualResult = Vector.map2i v1 v2 f

        let expectedResult =
            Vector.CoordinateList(dataLength, [ (0UL<index>, 2); (1UL<index>, 30); (2UL<index>, 8); (3UL<index>, 270) ])
            |> Vector.fromCoordinateList

        match actualResult, expectedResult with
        | Result.Ok actual, Result.Ok expected -> Assert.Equal(expected, actual)
        | _ -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple Vector.map2Values.`` () =
    let v1 =
        let tree =
            Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1))), Vector.btree.Leaf(UserValue(Some(2))))

        let store = Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 2UL<nvals>, store)

    let v2 =
        let tree =
            Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(10))), Vector.btree.Leaf(UserValue(Some(20))))

        let store = Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 2UL<nvals>, store)

    let f a b = Some(a + b)
    let actualResult = Vector.map2Values v1 v2 f

    let expectedResult =
        Vector.fromCoordinateList (
            Vector.CoordinateList(
                4UL<dataLength>,
                [ (0UL<index>, 11); (1UL<index>, 11); (2UL<index>, 22); (3UL<index>, 22) ]
            )
        )

    match actualResult, expectedResult with
    | Result.Ok actual, Result.Ok expected -> Assert.Equal(expected, actual)
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple Vector.map2AllCells.`` () =
    let v1 =
        let tree = Vector.btree.Leaf(UserValue(Some(5)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let v2 =
        let tree = Vector.btree.Leaf(UserValue(Some(3)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let f (x: Option<int>) (y: Option<int>) = Option.map2 (+) x y

    let expected =
        let tree = Vector.btree.Leaf(UserValue(Some(8)))
        let store = Storage(1UL<storageSize>, tree)
        Ok(SparseVector(1UL<dataLength>, 1UL<nvals>, store))

    let actual = Vector.map2AllCells v1 v2 f
    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.map2AtLeastOne.`` () =
    let v1 =
        let tree = Vector.btree.Leaf(UserValue(Some(5)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let v2 =
        let tree = Vector.btree.Leaf(UserValue(None))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 0UL<nvals>, store)

    let f (x: AtLeastOne<int, int>) =
        match x with
        | AtLeastOne.Both(a, b) -> Some(a + b)
        | AtLeastOne.Left a -> Some(a * 2)
        | AtLeastOne.Right b -> Some(b * 3)

    let expected =
        let tree = Vector.btree.Leaf(UserValue(Some(10)))
        let store = Storage(1UL<storageSize>, tree)
        Ok(SparseVector(1UL<dataLength>, 1UL<nvals>, store))

    let actual = Vector.map2AtLeastOne v1 v2 f
    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.map2LeftValues.`` () =
    let v1 =
        let tree = Vector.btree.Leaf(UserValue(Some(5)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let v2 =
        let tree = Vector.btree.Leaf(UserValue(None))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 0UL<nvals>, store)

    let f a (y: Option<int>) = Some(a + (defaultArg y 0))

    let expected =
        let tree = Vector.btree.Leaf(UserValue(Some(5)))
        let store = Storage(1UL<storageSize>, tree)
        Ok(SparseVector(1UL<dataLength>, 1UL<nvals>, store))

    let actual = Vector.map2LeftValues v1 v2 f
    Assert.Equal(expected, actual)

[<Fact>]
let ``Vector.map2Values compressed`` () =
    let dataLength = 5UL<dataLength>

    let d1 =
        [ 0UL<index>, 1; 1UL<index>, 1; 2UL<index>, 1; 3UL<index>, 1; 4UL<index>, 1 ]

    let d2 =
        [ 0UL<index>, 2; 1UL<index>, 3; 2UL<index>, 4; 3UL<index>, 5; 4UL<index>, 6 ]

    match
        Vector.fromCoordinateList (CoordinateList(dataLength, d1)),
        Vector.fromCoordinateList (CoordinateList(dataLength, d2))
    with
    | Result.Ok v1, Result.Ok v2 ->
        let f a b = Some(a + b)
        let actual = Vector.map2Values v1 v2 f

        let expectedResult =
            let data =
                [ 0UL<index>, 3; 1UL<index>, 4; 2UL<index>, 5; 3UL<index>, 6; 4UL<index>, 7 ]

            Vector.fromCoordinateList (CoordinateList(dataLength, data))

        match actual, expectedResult with
        | Result.Ok actual, Result.Ok expected -> Assert.Equal(expected, actual)
        | _ -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``Vector.map2Values compressed to None`` () =
    let dataLength = 5UL<dataLength>

    let d1 =
        [ 0UL<index>, 1; 1UL<index>, 1; 2UL<index>, 1; 3UL<index>, 1; 4UL<index>, 1 ]

    let d2 =
        [ 0UL<index>, 2; 1UL<index>, 3; 2UL<index>, 4; 3UL<index>, 5; 4UL<index>, 6 ]

    match
        Vector.fromCoordinateList (CoordinateList(dataLength, d1)),
        Vector.fromCoordinateList (CoordinateList(dataLength, d2))
    with
    | Result.Ok v1, Result.Ok v2 ->
        let f a b = None
        let actual = Vector.map2Values v1 v2 f

        let expected = Ok(Vector.empty dataLength)

        Assert.Equal(expected, actual)
    | _ -> Assert.Fail()

[<Fact>]
let ``Vector.map2Values compressed both`` () =
    let dataLength = 5UL<dataLength>

    let d1 =
        [ 0UL<index>, 1; 1UL<index>, 1; 2UL<index>, 1; 3UL<index>, 1; 4UL<index>, 1 ]

    let d2 =
        [ 0UL<index>, 2; 1UL<index>, 2; 2UL<index>, 2; 3UL<index>, 2; 4UL<index>, 2 ]

    match
        Vector.fromCoordinateList (CoordinateList(dataLength, d1)),
        Vector.fromCoordinateList (CoordinateList(dataLength, d2))
    with
    | Result.Ok v1, Result.Ok v2 ->
        let f a b = Some(a + b)
        let actual = Vector.map2Values v1 v2 f

        let expectedResult =
            let data =
                [ 0UL<index>, 3; 1UL<index>, 3; 2UL<index>, 3; 3UL<index>, 3; 4UL<index>, 3 ]

            Vector.fromCoordinateList (CoordinateList(dataLength, data))

        match actual, expectedResult with
        | Result.Ok actual, Result.Ok expected -> Assert.Equal(expected, actual)
        | _ -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``Vector.map2Values compressed both indexed`` () =
    let dataLength = 5UL<dataLength>

    let d1 =
        [ 0UL<index>, 1; 1UL<index>, 1; 2UL<index>, 1; 3UL<index>, 1; 4UL<index>, 1 ]

    let d2 =
        [ 0UL<index>, 2; 1UL<index>, 2; 2UL<index>, 2; 3UL<index>, 2; 4UL<index>, 2 ]

    match
        Vector.fromCoordinateList (CoordinateList(dataLength, d1)),
        Vector.fromCoordinateList (CoordinateList(dataLength, d2))
    with
    | Result.Ok v1, Result.Ok v2 ->
        let f i a b = Some(int i + a + b)
        let actual = Vector.map2iValues v1 v2 f

        let expectedResult =
            let data =
                [ 0UL<index>, 3; 1UL<index>, 4; 2UL<index>, 5; 3UL<index>, 6; 4UL<index>, 7 ]

            Vector.fromCoordinateList (CoordinateList(dataLength, data))

        match actual, expectedResult with
        | Result.Ok actual, Result.Ok expected -> Assert.Equal(expected, actual)
        | _ -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``Vector.map2Values compressed both to None`` () =
    let dataLength = 5UL<dataLength>

    let d1 =
        [ 0UL<index>, 1; 1UL<index>, 1; 2UL<index>, 1; 3UL<index>, 1; 4UL<index>, 1 ]

    let d2 =
        [ 0UL<index>, 2; 1UL<index>, 2; 2UL<index>, 2; 3UL<index>, 2; 4UL<index>, 2 ]

    match
        Vector.fromCoordinateList (CoordinateList(dataLength, d1)),
        Vector.fromCoordinateList (CoordinateList(dataLength, d2))
    with
    | Result.Ok v1, Result.Ok v2 ->
        let f a b = None
        let actual = Vector.map2Values v1 v2 f

        let expected = Ok(Vector.empty dataLength)

        Assert.Equal(expected, actual)
    | _ -> Assert.Fail()

[<Fact>]
let ``Vector.map2Values with None returns None`` () =
    let v1 =
        let tree = Vector.btree.Leaf(UserValue(None))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 0UL<nvals>, store)

    let v2 =
        let tree = Vector.btree.Leaf(UserValue(Some(3)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let f a b = Some(a + b)

    let expected =
        let tree = Vector.btree.Leaf(UserValue(None))
        let store = Storage(1UL<storageSize>, tree)
        Ok(SparseVector(1UL<dataLength>, 0UL<nvals>, store))

    let actual = Vector.map2Values v1 v2 f
    Assert.Equal(expected, actual)

[<Fact>]
let ``Vector.map2AtLeastOne Both case`` () =
    let v1 =
        let tree = Vector.btree.Leaf(UserValue(Some(5)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let v2 =
        let tree = Vector.btree.Leaf(UserValue(Some(3)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let f (x: AtLeastOne<int, int>) =
        match x with
        | AtLeastOne.Both(a, b) -> Some(a + b)
        | AtLeastOne.Left a -> Some(a)
        | AtLeastOne.Right b -> Some(b)

    let expected =
        let tree = Vector.btree.Leaf(UserValue(Some(8)))
        let store = Storage(1UL<storageSize>, tree)
        Ok(SparseVector(1UL<dataLength>, 1UL<nvals>, store))

    let actual = Vector.map2AtLeastOne v1 v2 f
    Assert.Equal(expected, actual)

[<Fact>]
let ``Vector.map2AtLeastOne Right case`` () =
    let v1 =
        let tree = Vector.btree.Leaf(UserValue(None))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 0UL<nvals>, store)

    let v2 =
        let tree = Vector.btree.Leaf(UserValue(Some(7)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let f (x: AtLeastOne<int, int>) =
        match x with
        | AtLeastOne.Both(a, b) -> Some(a + b)
        | AtLeastOne.Left a -> Some(a)
        | AtLeastOne.Right b -> Some(b * 2)

    let expected =
        let tree = Vector.btree.Leaf(UserValue(Some(14)))
        let store = Storage(1UL<storageSize>, tree)
        Ok(SparseVector(1UL<dataLength>, 1UL<nvals>, store))

    let actual = Vector.map2AtLeastOne v1 v2 f
    Assert.Equal(expected, actual)

[<Fact>]
let ``Conversion identity`` () =
    let dataLength = 10UL<dataLength>

    let data =
        [ 0UL<index>, 3; 3UL<index>, -1; 7UL<index>, 2; 8UL<index>, 2; 9UL<index>, 2 ]

    let coordinates = CoordinateList(dataLength, data)

    match fromCoordinateList coordinates with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let actual = toCoordinateList vec
        Assert.Equal(coordinates, actual)

[<Fact>]
let ``Simple addition`` () =
    let dataLength = 10UL<dataLength>

    let d1 = [ 0UL<index>, 2; 9UL<index>, 1 ]
    let d2 = [ 0UL<index>, 3; 8UL<index>, 1 ]

    let expected =
        let expectedList = [ 0UL<index>, 5; 8UL<index>, 1; 9UL<index>, 1 ]
        CoordinateList(dataLength, expectedList)

    match fromCoordinateList (CoordinateList(dataLength, d1)) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok v1 ->
        match fromCoordinateList (CoordinateList(dataLength, d2)) with
        | Result.Error msg -> Assert.Fail()
        | Result.Ok v2 ->
            let addition o1 o2 =
                match o1, o2 with
                | Some x, Some y -> Some(x + y)
                | Some x, None
                | None, Some x -> Some x
                | None, None -> None

            match map2 v1 v2 addition with
            | Ok resultVector ->
                let actual = toCoordinateList resultVector
                Assert.Equal(expected, actual)
            | Error msg -> Assert.Fail()

[<Fact>]
let ``Condensation of empty`` () =
    let clist = CoordinateList(10UL<dataLength>, [])

    match fromCoordinateList clist with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok actual ->
        // 16 elements total None and Dummy: NNNNNNNN | NN DD | DDDD
        let tree =
            btree.Node(
                btree.Leaf <| UserValue None,
                btree.Node(btree.Node(btree.Leaf <| UserValue None, btree.Leaf Dummy), btree.Leaf Dummy)
            )

        let expectedResult =
            SparseVector(clist.length, 0UL<nvals>, Storage(16UL<storageSize>, tree))

        Assert.Equal(Ok expectedResult, Ok actual)

[<Fact>]
let ``Gather`` () =
    match
        Vector.fromCoordinateList (
            Vector.CoordinateList(5UL<dataLength>, [ (0UL<index>, 0.0); (1UL<index>, 1.0); (4UL<index>, 5.0) ])
        )
    with
    | Error _ -> Assert.Fail()
    | Ok data ->
        match
            Vector.fromCoordinateList (
                Vector.CoordinateList(
                    5UL<dataLength>,
                    [ (0UL<index>, 1UL<index>); (1UL<index>, 4UL<index>); (3UL<index>, 1UL<index>) ]
                )
            )
        with
        | Error _ -> Assert.Fail()
        | Ok indices ->
            let actual = Vector.gather data indices

            match
                Vector.fromCoordinateList (
                    Vector.CoordinateList(5UL<dataLength>, [ (0UL<index>, 1.0); (1UL<index>, 5.0); (3UL<index>, 1.0) ])
                )
            with
            | Error _ -> Assert.Fail()
            | Ok expected -> Assert.Equal(expected, actual)

[<Fact>]
let ``Scatter`` () =
    let data =
        Vector.fromCoordinateList (Vector.CoordinateList(5UL<dataLength>, [ (0UL<index>, 4.0); (2UL<index>, 5.0) ]))

    let indices =
        Vector.fromCoordinateList (
            Vector.CoordinateList(5UL<dataLength>, [ (0UL<index>, 3UL<index>); (2UL<index>, 3UL<index>) ])
        )

    let result =
        Vector.fromCoordinateList (Vector.CoordinateList(5UL<dataLength>, [ (3UL<index>, 1.0); (4UL<index>, 3.0) ]))

    match data, indices, result with
    | Ok dataOk, Ok indicesOk, Ok resultOk ->
        let actual =
            Vector.scatter resultOk dataOk indicesOk (fun x y ->
                match (x, y) with
                | Some x, Some y -> Some(x + y)
                | Some x, _
                | _, Some x -> Some x
                | _ -> None)

        let expected =
            Vector.CoordinateList(5UL<dataLength>, [ (3UL<index>, 10.0); (4UL<index>, 3.0) ])
            |> Vector.fromCoordinateList

        match expected with
        | Ok expectedOk -> Assert.Equal(Ok expectedOk, actual)
        | Error msg -> Assert.Fail()
    | Error msg, _, _ -> Assert.Fail()
    | _, Error msg, _ -> Assert.Fail()
    | _, _, Error msg -> Assert.Fail()

let compare x y =
    match (x, y) with
    | Some x, None -> -1
    | Some x, Some y ->
        if x < y then -1
        elif x > y then 1
        else 0
    | None, Some x -> 1
    | _ -> 0

[<Fact>]
let ``Sort one element vector`` () =
    let data =
        Vector.CoordinateList(1UL<dataLength>, [ (0UL<index>, 0.0) ])
        |> Vector.fromCoordinateList

    match data with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let actual = Vector.mergeSort vec compare
        Assert.Equal(data, Ok actual)

[<Fact>]
let ``Sort vector of two equal elements`` () =
    let data =
        Vector.CoordinateList(2UL<dataLength>, [ (0UL<index>, 0.0); (1UL<index>, 0.0) ])
        |> Vector.fromCoordinateList

    match data with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let actual = Vector.mergeSort vec compare
        Assert.Equal(data, Ok actual)

[<Fact>]
let ``Sort vector of three equal elements`` () =
    let data =
        Vector.CoordinateList(3UL<dataLength>, [ (0UL<index>, 2.0); (1UL<index>, 2.0); (2UL<index>, 2.0) ])
        |> Vector.fromCoordinateList

    match data with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let actual = Vector.mergeSort vec compare
        Assert.Equal(data, Ok actual)

[<Fact>]
let ``Sort vector of three different unordered elements`` () =
    let data =
        Vector.CoordinateList(3UL<dataLength>, [ (0UL<index>, 2.0); (1UL<index>, 1.0); (2UL<index>, 4.0) ])
        |> Vector.fromCoordinateList

    let expected =
        Vector.CoordinateList(3UL<dataLength>, [ (0UL<index>, 1.0); (1UL<index>, 2.0); (2UL<index>, 4.0) ])
        |> Vector.fromCoordinateList

    match data with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let actual = Vector.mergeSort vec compare
        Assert.Equal(expected, Ok actual)

[<Fact>]
let ``Sort long vector with one element`` () =
    let data =
        Vector.CoordinateList(5UL<dataLength>, [ (0UL<index>, 0.0) ])
        |> Vector.fromCoordinateList

    match data with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let actual = Vector.mergeSort vec compare
        Assert.Equal(data, Ok actual)

[<Fact>]
let ``Sort sorted vector`` () =
    let data =
        Vector.CoordinateList(5UL<dataLength>, [ (0UL<index>, 0.0); (1UL<index>, 0.0) ])
        |> Vector.fromCoordinateList

    match data with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let actual = Vector.mergeSort vec compare
        Assert.Equal(data, Ok actual)

[<Fact>]
let ``Init vector`` () =
    let expected =
        Vector.CoordinateList(3UL<dataLength>, [ (0UL<index>, 0); (1UL<index>, 1); (2UL<index>, 2) ])
        |> fromCoordinateListUnchecked

    let actual = Vector.init 3UL<dataLength> (fun i -> Some(int i))
    Assert.Equal(expected, actual)



[<Fact>]
let ``map on empty vector returns empty vector`` () =
    match fromCoordinateList (CoordinateList(0UL<dataLength>, [])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let result =
            map vec (fun (x: int option) ->
                match x with
                | Some v -> Some(v * 3)
                | None -> None)

        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(0UL<dataLength>, []), coo)

[<Fact>]
let ``map with function that turns all the elements into zeros`` () =
    match
        fromCoordinateList (CoordinateList(7UL<dataLength>, [ (1UL<index>, 3); (2UL<index>, 3); (5UL<index>, 100) ]))
    with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let result = map vec (fun _ -> Some 0)
        let coo = toCoordinateList result

        Assert.Equal(
            CoordinateList(
                7UL<dataLength>,
                [ (0UL<index>, 0)
                  (1UL<index>, 0)
                  (2UL<index>, 0)
                  (3UL<index>, 0)
                  (4UL<index>, 0)
                  (5UL<index>, 0)
                  (6UL<index>, 0) ]
            ),
            coo
        )

[<Fact>]
let ``map with function that resets all the elements`` () =
    match
        fromCoordinateList (CoordinateList(7UL<dataLength>, [ (1UL<index>, 3); (2UL<index>, 3); (5UL<index>, 100) ]))
    with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let result = map vec (fun _ -> None)
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(7UL<dataLength>, []), coo)

[<Fact>]
let ``map can change type from int to string`` () =
    match fromCoordinateList (CoordinateList(3UL<dataLength>, [ (1UL<index>, 11); (2UL<index>, 33) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let result =
            map vec (fun (x: int option) ->
                match x with
                | Some v -> Some(sprintf "str %d" v)
                | None -> None)

        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(3UL<dataLength>, [ (1UL<index>, "str 11"); (2UL<index>, "str 33") ]), coo)

[<Fact>]
let ``fromCoordinateList with out-of-range index returns error`` () =
    let coo = CoordinateList(6UL<dataLength>, [ (9UL<index>, 8) ])
    let expected = Error "Index out of range"
    Assert.Equal(expected, fromCoordinateList coo)

[<Fact>]
let ``fromCoordinateList with zero size returns error`` () =
    let coo = CoordinateList(0UL<dataLength>, [ (33UL<index>, 33); (39UL<index>, 1) ])
    let expected = Error "Index out of range"
    Assert.Equal(expected, fromCoordinateList coo)

[<Fact>]
let ``fromCoordinateList with unsorted coordinates works correctly`` () =
    let coo =
        CoordinateList(7UL<dataLength>, [ (5UL<index>, 3); (3UL<index>, 2); (1UL<index>, 100) ])

    match fromCoordinateList coo with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let result = toCoordinateList vec
        Assert.Equal(CoordinateList(7UL<dataLength>, [ (1UL<index>, 100); (3UL<index>, 2); (5UL<index>, 3) ]), result)

[<Fact>]
let ``fromCoordinateList with duplicate indicies returns the last of them`` () =
    let coo = CoordinateList(3UL<dataLength>, [ (1UL<index>, 33); (1UL<index>, 100) ])

    match Vector.fromCoordinateList coo with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let result = Vector.toCoordinateList vec
        Assert.Equal(CoordinateList(3UL<dataLength>, [ (1UL<index>, 100) ]), result)

[<Fact>]
let ``mapi works with index`` () =
    match fromCoordinateList (CoordinateList(5UL<dataLength>, [ (2UL<index>, 10); (3UL<index>, 23) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let result =
            mapi vec (fun (i: uint64<index>) (x: int option) ->
                match x with
                | Some v -> Some(int i + v)
                | None -> None)

        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(5UL<dataLength>, [ (2UL<index>, 12); (3UL<index>, 26) ]), coo)

[<Fact>]
let ``mapi on empty vector returns empty vector`` () =
    match fromCoordinateList (CoordinateList(0UL<dataLength>, [])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let result =
            mapi vec (fun (i: uint64<index>) (x: int option) ->
                match x with
                | Some v -> Some(int i + v)
                | None -> None)

        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(0UL<dataLength>, []), coo)

[<Fact>]
let ``mapi with special function returns empty vector`` () =
    match fromCoordinateList (CoordinateList(5UL<dataLength>, [ (1UL<index>, 33); (3UL<index>, 88) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let result =
            mapi vec (fun (i: uint64<index>) (x: int option) ->
                match x with
                | Some v ->
                    match int i % 2 with
                    | 0 -> Some(v * 2)
                    | _ -> None
                | None -> None)

        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(5UL<dataLength>, []), coo)

[<Fact>]
let ``mapi works with function does not depend on the index`` () =
    match fromCoordinateList (CoordinateList(5UL<dataLength>, [ (2UL<index>, 10); (3UL<index>, 23) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        let result =
            mapi vec (fun (i: uint64<index>) (x: int option) ->
                match x with
                | Some v -> Some(v + 4)
                | None -> None)

        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(5UL<dataLength>, [ (2UL<index>, 14); (3UL<index>, 27) ]), coo)

[<Fact>]
let ``slice returns error when start is negative`` () =
    match fromCoordinateList (CoordinateList(5UL<dataLength>, [ (2UL<index>, 10) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice -1 3 vec with
        | Result.Ok _ -> Assert.Fail("Expected Error")
        | Result.Error msg -> Assert.Equal("Start should be >= 0", msg)

[<Fact>]
let ``slice returns error when end is negative`` () =
    match fromCoordinateList (CoordinateList(5UL<dataLength>, [ (2UL<index>, 10) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice 1 -3 vec with
        | Result.Ok _ -> Assert.Fail("Expected Error")
        | Result.Error msg -> Assert.Equal("End should be >= 0", msg)

[<Fact>]
let ``slice returns error when start is out of range`` () =
    match fromCoordinateList (CoordinateList(5UL<dataLength>, [ (2UL<index>, 10) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice 7 3 vec with
        | Result.Ok _ -> Assert.Fail("Expected Error")
        | Result.Error msg -> Assert.Equal("Start is out of Vector length", msg)

[<Fact>]
let ``slice returns error when end is out of range`` () =
    match fromCoordinateList (CoordinateList(5UL<dataLength>, [ (2UL<index>, 10) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice 3 7 vec with
        | Result.Ok _ -> Assert.Fail("Expected Error")
        | Result.Error msg -> Assert.Equal("End is out of Vector length", msg)

[<Fact>]
let ``slice returns error when end is less than start`` () =
    match fromCoordinateList (CoordinateList(5UL<dataLength>, [ (2UL<index>, 10) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice 4 3 vec with
        | Result.Ok _ -> Assert.Fail("Expected Error")
        | Result.Error msg -> Assert.Equal("End should be >= Start", msg)

[<Fact>]
let ``slice returns correct subvector`` () =
    match fromCoordinateList (CoordinateList(7UL<dataLength>, [ (2UL<index>, 10); (6UL<index>, 20) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice 1 3 vec with
        | Result.Ok result ->
            let coo = Vector.toCoordinateList result
            Assert.Equal(CoordinateList(3UL<dataLength>, [ (1UL<index>, 10) ]), coo)
        | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns subvector without elements`` () =
    match fromCoordinateList (CoordinateList(7UL<dataLength>, [ (5UL<index>, 10); (6UL<index>, 20) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice 0 2 vec with
        | Result.Ok result ->
            let coo = toCoordinateList result
            Assert.Equal(CoordinateList(3UL<dataLength>, []), coo)
        | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns single subvector`` () =
    match fromCoordinateList (CoordinateList(7UL<dataLength>, [ (2UL<index>, 10); (6UL<index>, 20) ])) with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice 2 2 vec with
        | Result.Ok result ->
            let coo = toCoordinateList result
            Assert.Equal(CoordinateList(1UL<dataLength>, [ (0UL<index>, 10) ]), coo)
        | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns correct subvector equals to vector`` () =
    match
        fromCoordinateList (CoordinateList(7UL<dataLength>, [ (2UL<index>, 10); (3UL<index>, 33); (6UL<index>, 20) ]))
    with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice 0 6 vec with
        | Result.Ok result ->
            let coo = toCoordinateList result
            Assert.Equal(CoordinateList(7UL<dataLength>, [ (2UL<index>, 10); (3UL<index>, 33); (6UL<index>, 20) ]), coo)
        | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns correct subvector when start of subvector equals to start of vector`` () =
    match
        fromCoordinateList (CoordinateList(7UL<dataLength>, [ (0UL<index>, 10); (3UL<index>, 33); (6UL<index>, 20) ]))
    with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice 0 4 vec with
        | Result.Ok result ->
            let coo = Vector.toCoordinateList result
            Assert.Equal(CoordinateList(5UL<dataLength>, [ (0UL<index>, 10); (3UL<index>, 33) ]), coo)
        | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns correct subvector when end of subvector equals to end of vector`` () =
    match
        fromCoordinateList (CoordinateList(7UL<dataLength>, [ (0UL<index>, 10); (3UL<index>, 33); (6UL<index>, 20) ]))
    with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok vec ->
        match slice 2 6 vec with
        | Result.Ok result ->
            let coo = Vector.toCoordinateList result
            Assert.Equal(CoordinateList(5UL<dataLength>, [ (1UL<index>, 33); (4UL<index>, 20) ]), coo)
        | Result.Error msg -> Assert.Fail(msg)
