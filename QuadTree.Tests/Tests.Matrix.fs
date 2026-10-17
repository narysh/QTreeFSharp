module Matrix.Tests

open OptionMonoid

open System
open Xunit

open Matrix
open COOArray
open Common

let private fromCoordinateListUnchecked (lst: Matrix.CoordinateList<'a>) =
    match Matrix.fromCoordinateList lst with
    | Ok m -> m
    | Error e -> failwith e

let printMatrix (matrix: SparseMatrix<_>) =
    printfn "Matrix:"
    printfn "   Rows: %A" matrix.nrows
    printfn "   Columns: %A" matrix.ncols
    printfn "   Nvals: %A" matrix.nvals
    printfn "   Storage:"
    printfn "      size: %A" matrix.storage.size
    printfn "      Data: %A" matrix.storage.data

let printMatrixCoordinate (matrix: SparseMatrix<_>) =
    printfn "Matrix:"
    printfn "   Rows: %A" matrix.nrows
    printfn "   Columns: %A" matrix.ncols
    printfn "   Nvals: %A" matrix.nvals
    printfn "   Storage:"
    printfn "      size: %A" matrix.storage.size
    printfn "      Data: %A" (Matrix.toCoordinateList matrix).list


let leaf_v v = qtree.Leaf << UserValue <| Some v
let leaf_n () = qtree.Leaf << UserValue <| None
let leaf_d () = qtree.Leaf Dummy


(*
N,1,1,N
3,2,2,3
N,N,1,2
N,N,3,N
+
1,1,2,2
1,1,2,2
3,3,N,N
3,3,N,N
=
N,2,3,N
4,3,4,5
N,N,N,N
N,N,N,N
*)
[<Fact>]
let ``Simple Matrix.map2. Square where number of cols and rows are power of two.`` () =
    let m1 =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(leaf_n (), leaf_v 1, leaf_v 3, leaf_v 2),
                Matrix.qtree.Node(leaf_v 1, leaf_n (), leaf_v 2, leaf_v 3),
                leaf_n (),
                Matrix.qtree.Node(leaf_v 1, leaf_v 2, leaf_v 3, leaf_n ())
            )

        let store = Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 9UL<nvals>, store)

    let m2 =
        let tree = Matrix.qtree.Node(leaf_v 1, leaf_v 2, leaf_v 3, leaf_n ())

        let store = Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 12UL<nvals>, store)

    let f x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | _ -> None

    let expected =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(leaf_n (), leaf_v 2, leaf_v 4, leaf_v 3),
                Matrix.qtree.Node(leaf_v 3, leaf_n (), leaf_v 4, leaf_v 5),
                leaf_n (),
                leaf_n ()
            )

        let store = Storage(4UL<storageSize>, tree)
        Ok(SparseMatrix(4UL<nrows>, 4UL<ncols>, 6UL<nvals>, store))

    let actual = Matrix.map2 m1 m2 f

    Assert.Equal(expected, actual)

(*
N,1,1,D
3,2,2,D
N,N,1,D
D,D,D,D
+
1,1,2,D
1,1,2,D
3,3,N,D
D,D,D,D
=
N,2,3,D
4,3,4,D
N,N,N,D
D,D,D,D
*)

[<Fact>]
let ``Simple Matrix.map2. Square where number of cols and rows are not power of two.`` () =
    let m1 =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(leaf_n (), leaf_v 1, leaf_v 3, leaf_v 2),
                Matrix.qtree.Node(leaf_v 1, leaf_d (), leaf_v 2, leaf_d ()),
                Matrix.qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
                Matrix.qtree.Node(leaf_v 1, leaf_d (), leaf_d (), leaf_d ())
            )

        let store = Storage(4UL<storageSize>, tree)
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 6UL<nvals>, store)

    let m2 =
        let tree =
            Matrix.qtree.Node(
                leaf_v 1,
                Matrix.qtree.Node(leaf_v 2, leaf_d (), leaf_v 2, leaf_d ()),
                Matrix.qtree.Node(leaf_v 3, leaf_v 3, leaf_d (), leaf_d ()),
                Matrix.qtree.Node(leaf_n (), leaf_d (), leaf_d (), leaf_d ())
            )

        let store = Storage(4UL<storageSize>, tree)
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 8UL<nvals>, store)

    let f x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | _ -> None

    let expected =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(leaf_n (), leaf_v 2, leaf_v 4, leaf_v 3),
                Matrix.qtree.Node(leaf_v 3, leaf_d (), leaf_v 4, leaf_d ()),
                Matrix.qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
                Matrix.qtree.Node(leaf_n (), leaf_d (), leaf_d (), leaf_d ())
            )

        let store = Storage(4UL<storageSize>, tree)
        Ok(SparseMatrix(3UL<nrows>, 3UL<ncols>, 5UL<nvals>, store))

    let actual = Matrix.map2 m1 m2 f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Matrix.map2i. Square where number of cols and rows are power of two.`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 2)
          (1UL<rowindex>, 0UL<colindex>, 3)
          (1UL<rowindex>, 1UL<colindex>, 4) ]

    let d2 =
        [ (0UL<rowindex>, 0UL<colindex>, 10)
          (0UL<rowindex>, 1UL<colindex>, 20)
          (1UL<rowindex>, 0UL<colindex>, 30)
          (1UL<rowindex>, 1UL<colindex>, 40) ]

    match
        Matrix.fromCoordinateList (Matrix.CoordinateList(nrows, ncols, d1)),
        Matrix.fromCoordinateList (Matrix.CoordinateList(nrows, ncols, d2))
    with
    | Result.Ok m1, Result.Ok m2 ->
        let f row col x y =
            match (x, y) with
            | Some(a), Some(b) -> Some(a + b + int row + int col)
            | _ -> None

        let actualResult = Matrix.map2i m1 m2 f

        let expectedResult =
            Matrix.CoordinateList(
                nrows,
                ncols,
                [ (0UL<rowindex>, 0UL<colindex>, 11)
                  (0UL<rowindex>, 1UL<colindex>, 23)
                  (1UL<rowindex>, 0UL<colindex>, 34)
                  (1UL<rowindex>, 1UL<colindex>, 46) ]
            )
            |> Matrix.fromCoordinateList

        match actualResult, expectedResult with
        | Result.Ok actual, Result.Ok expected -> Assert.Equal(expected, actual)
        | _ -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple Matrix.map2i. Square where number of cols and rows are not power of two.`` () =
    let nrows = 3UL<nrows>
    let ncols = 3UL<ncols>

    let d1 =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 2)
          (0UL<rowindex>, 2UL<colindex>, 3)
          (1UL<rowindex>, 0UL<colindex>, 4)
          (1UL<rowindex>, 1UL<colindex>, 5)
          (1UL<rowindex>, 2UL<colindex>, 6) ]

    let d2 =
        [ (0UL<rowindex>, 0UL<colindex>, 10)
          (0UL<rowindex>, 1UL<colindex>, 10)
          (0UL<rowindex>, 2UL<colindex>, 10)
          (1UL<rowindex>, 0UL<colindex>, 10)
          (1UL<rowindex>, 1UL<colindex>, 10)
          (1UL<rowindex>, 2UL<colindex>, 10) ]

    match
        Matrix.fromCoordinateList (Matrix.CoordinateList(nrows, ncols, d1)),
        Matrix.fromCoordinateList (Matrix.CoordinateList(nrows, ncols, d2))
    with
    | Result.Ok m1, Result.Ok m2 ->
        let f row col x y =
            match (x, y) with
            | Some(a), Some(b) -> Some(a * (int row + 1) + b * (int col + 1))
            | _ -> None

        let actual = Matrix.map2i m1 m2 f

        match actual with
        | Result.Ok _ -> ()
        | Result.Error _ -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple Matrix.map2i. Mixed values.`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 3) ]
    let d2 = [ (1UL<rowindex>, 1UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 30) ]

    match
        Matrix.fromCoordinateList (Matrix.CoordinateList(nrows, ncols, d1)),
        Matrix.fromCoordinateList (Matrix.CoordinateList(nrows, ncols, d2))
    with
    | Result.Ok m1, Result.Ok m2 ->
        let f row col x y =
            match (x, y) with
            | Some(a), Some(b) -> Some(a + b)
            | Some(a), None -> Some(int col + a * 2)
            | None, Some(b) -> Some(int row + b * 3)
            | _ -> None

        let actualResult = Matrix.map2i m1 m2 f

        let expectedResult =
            Matrix.CoordinateList(
                nrows,
                ncols,
                [ (0UL<rowindex>, 0UL<colindex>, 2)
                  (1UL<rowindex>, 1UL<colindex>, 31)
                  (2UL<rowindex>, 2UL<colindex>, 8)
                  (3UL<rowindex>, 3UL<colindex>, 93) ]
            )
            |> Matrix.fromCoordinateList

        match actualResult, expectedResult with
        | Result.Ok actual, Result.Ok expected -> Assert.Equal(expected, actual)
        | _ -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple Matrix.mapi. Square where number of cols and rows are power of two.`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 2)
          (1UL<rowindex>, 0UL<colindex>, 3)
          (1UL<rowindex>, 1UL<colindex>, 4) ]

    match Matrix.fromCoordinateList (Matrix.CoordinateList(nrows, ncols, d)) with
    | Result.Ok m ->
        let f row col x =
            match x with
            | Some(a) -> Some(a + int row + int col)
            | _ -> None

        let actual = Matrix.mapi m f
        Assert.Equal(4UL<nvals>, actual.nvals)
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple Matrix.mapi. Square where number of cols and rows are not power of two.`` () =
    let nrows = 3UL<nrows>
    let ncols = 3UL<ncols>

    let d =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 2)
          (0UL<rowindex>, 2UL<colindex>, 3)
          (1UL<rowindex>, 0UL<colindex>, 4)
          (1UL<rowindex>, 1UL<colindex>, 5)
          (1UL<rowindex>, 2UL<colindex>, 6) ]

    match Matrix.fromCoordinateList (Matrix.CoordinateList(nrows, ncols, d)) with
    | Result.Ok m ->
        let f row col x =
            match x with
            | Some(a) -> Some(a * (int row + 1) * (int col + 1))
            | _ -> None

        let actual = Matrix.mapi m f
        Assert.Equal(6UL<nvals>, actual.nvals)
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple Matrix.mapi. Multiply row index by value.`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (1UL<rowindex>, 1UL<colindex>, 2)
          (2UL<rowindex>, 2UL<colindex>, 3)
          (3UL<rowindex>, 3UL<colindex>, 4) ]

    match Matrix.fromCoordinateList (Matrix.CoordinateList(nrows, ncols, d)) with
    | Result.Ok m ->
        let f row col x =
            match x with
            | Some(a) -> Some(a * int row)
            | _ -> None

        let actual = Matrix.mapi m f
        Assert.Equal(4UL<nvals>, actual.nvals)
    | _ -> Assert.Fail()

[<Fact>]
let ``Conversion identity`` () =
    let nrows = 10UL<nrows>
    let ncols = 12UL<ncols>

    let data =
        [ 0UL<rowindex>, 3UL<colindex>, 10
          3UL<rowindex>, 3UL<colindex>, 33
          9UL<rowindex>, 2UL<colindex>, 5
          3UL<rowindex>, 11UL<colindex>, 1 ]
        |> List.sort

    let coordinates = Matrix.CoordinateList(nrows, ncols, data)

    match Matrix.fromCoordinateList coordinates with
    | Result.Ok m ->
        let actual = Matrix.toCoordinateList m
        Assert.Equal(coordinates, actual)
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple addition`` () =
    let nrows = 10UL<nrows>
    let ncols = 12UL<ncols>

    let d1 =
        [ 0UL<rowindex>, 3UL<colindex>, 4
          9UL<rowindex>, 2UL<colindex>, 5
          3UL<rowindex>, 11UL<colindex>, 2 ]

    let d2 =
        [ 0UL<rowindex>, 3UL<colindex>, 6
          3UL<rowindex>, 3UL<colindex>, 33
          3UL<rowindex>, 11UL<colindex>, -1 ]

    let expected =
        CoordinateList(
            nrows,
            ncols,
            [ 0UL<rowindex>, 3UL<colindex>, 10
              3UL<rowindex>, 3UL<colindex>, 33
              9UL<rowindex>, 2UL<colindex>, 5
              3UL<rowindex>, 11UL<colindex>, 1 ]
            |> List.sort
        )

    match
        Matrix.fromCoordinateList (CoordinateList(nrows, ncols, d1)),
        Matrix.fromCoordinateList (CoordinateList(nrows, ncols, d2))
    with
    | Result.Ok m1, Result.Ok m2 ->
        let addition o1 o2 =
            match o1, o2 with
            | Some x, Some y -> Some(x + y)
            | Some x, None
            | None, Some x -> Some x
            | None, None -> None

        match Matrix.map2 m1 m2 addition with
        | Result.Ok resultMatrix ->
            let actual = Matrix.toCoordinateList resultMatrix
            Assert.Equal(expected, actual)
        | _ -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``Condensation of empty`` () =
    let clist = CoordinateList(2UL<nrows>, 3UL<ncols>, [])

    // 2 * 3 = 5
    // 4 * 4 None and Dummy
    // NN N D
    // NN N D
    // DDDD
    // DDDD

    match Matrix.fromCoordinateList clist with
    | Result.Ok actual ->
        let tree =
            qtree.Node(leaf_n (), qtree.Node(leaf_n (), leaf_d (), leaf_n (), leaf_d ()), leaf_d (), leaf_d ())

        let expected =
            SparseMatrix(2UL<nrows>, 3UL<ncols>, 0UL<nvals>, Storage(4UL<storageSize>, tree))

        Assert.Equal(expected.storage.data, actual.storage.data)
    | _ -> Assert.Fail()

[<Fact>]
let ``Condensation of sparse`` () =
    let clist =
        CoordinateList(4UL<nrows>, 3UL<ncols>, [ 0UL<rowindex>, 2UL<colindex>, 2; 3UL<rowindex>, 2UL<colindex>, 4 ])

    // NN2D
    // NNND
    // NNND
    // NN4D

    match Matrix.fromCoordinateList clist with
    | Result.Ok actual ->
        let tree =
            qtree.Node(
                leaf_n (),
                qtree.Node(leaf_v 2, leaf_d (), leaf_n (), leaf_d ()),
                leaf_n (),
                qtree.Node(leaf_n (), leaf_d (), leaf_v 4, leaf_d ())
            )

        let expected =
            SparseMatrix(4UL<nrows>, 3UL<ncols>, 2UL<nvals>, Storage(4UL<storageSize>, tree))

        Assert.Equal(expected.storage.data, actual.storage.data)
    | _ -> Assert.Fail()

[<Fact>]
let ``fold -> op_add`` () =
    // 222D
    // 222D
    // 222D
    // DDDD
    let tree =
        qtree.Node(
            leaf_v 2,
            qtree.Node(leaf_v 2, leaf_d (), leaf_v 2, leaf_d ()),
            qtree.Node(leaf_v 2, leaf_v 2, leaf_d (), leaf_d ()),
            qtree.Node(leaf_v 2, leaf_d (), leaf_d (), leaf_d ())
        )

    let m1 =
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 9UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let expected = 18

    let actual = Option.get <| Matrix.foldAssociative op_add None m1

    Assert.Equal(expected, actual)

[<Fact>]
let ``4x4 lower triangle`` () =
    // 2222
    // 2222
    // 2222
    // 2222
    let tree = leaf_v 2

    // 2NNN
    // 22NN
    // 222N
    // 2222
    let tree_expected =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_n (), leaf_v 2, leaf_v 2),
            leaf_n (),
            leaf_v 2,
            qtree.Node(leaf_v 2, leaf_n (), leaf_v 2, leaf_v 2)
        )

    let m1 =
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 16UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let expected =
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 10UL<nvals>, Matrix.Storage(4UL<storageSize>, tree_expected))

    let actual = getLowerTriangle m1

    Assert.Equal(expected, actual)

[<Fact>]
let ``3x3 lower triangle`` () =
    // 222 D
    // N22 D
    // NN2 D

    // DDD D
    let tree =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_v 2, leaf_n (), leaf_v 2),
            qtree.Node(leaf_v 2, leaf_d (), leaf_v 2, leaf_d ()),
            qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
            qtree.Node(leaf_v 2, leaf_d (), leaf_d (), leaf_d ())
        )


    // 2NN D
    // N2N D
    // NN2 D

    // DDD D
    let tree_expected =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_n (), leaf_n (), leaf_v 2),
            qtree.Node(leaf_n (), leaf_d (), leaf_n (), leaf_d ()),
            qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
            qtree.Node(leaf_v 2, leaf_d (), leaf_d (), leaf_d ())
        )

    let m1 =
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 6UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let expected =
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 3UL<nvals>, Matrix.Storage(4UL<storageSize>, tree_expected))

    let actual = getLowerTriangle m1

    Assert.Equal(expected, actual)

[<Fact>]
let ``2x3 transposition`` () =
    // 2N2D
    // N2ND
    // DDDD
    // DDDD
    let tree =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_n (), leaf_n (), leaf_v 2),
            qtree.Node(leaf_v 2, leaf_d (), leaf_n (), leaf_d ()),
            leaf_d (),
            leaf_d ()
        )


    // 2NDD
    // N2DD
    // 2NDD
    // DDDD
    let tree_expected =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_n (), leaf_n (), leaf_v 2),
            leaf_d (),
            qtree.Node(leaf_v 2, leaf_n (), leaf_d (), leaf_d ()),
            leaf_d ()
        )

    let m1 =
        SparseMatrix(2UL<nrows>, 3UL<ncols>, 3UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let expected =
        SparseMatrix(3UL<nrows>, 2UL<ncols>, 3UL<nvals>, Matrix.Storage(4UL<storageSize>, tree_expected))

    let actual = transpose m1

    Assert.Equal(expected, actual)

[<Fact>]
let ``Fold op_add`` () =
    // 2N2D
    // N2ND
    // DDDD
    // DDDD
    let tree =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_n (), leaf_n (), leaf_v 2),
            qtree.Node(leaf_v 2, leaf_d (), leaf_n (), leaf_d ()),
            leaf_d (),
            leaf_d ()
        )

    let m1 =
        SparseMatrix(2UL<nrows>, 3UL<ncols>, 3UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let expected = 6

    let actual = foldAssociative op_add None m1 |> Option.get

    Assert.Equal(expected, actual)

[<Fact>]
let ``matrix get existing value`` () =
    let m =
        fromCoordinateListUnchecked (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 7); (1UL<rowindex>, 2UL<colindex>, 9) ]
            )
        )

    Assert.Equal(Ok(Some 7), get m 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(Some 9), get m 1UL<rowindex> 2UL<colindex>)

[<Fact>]
let ``matrix get missing value`` () =
    let m =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 7) ]))

    Assert.Equal(Ok None, get m 1UL<rowindex> 1UL<colindex>)

[<Fact>]
let ``matrix get out of bounds`` () =
    let m = fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, []))
    Assert.Equal(Error Error.InconsistentSizeOfArguments, get m 5UL<rowindex> 5UL<colindex>)

[<Fact>]
let ``matrix set replaces existing`` () =
    let m =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 7) ]))

    let actual = set m 0UL<rowindex> 0UL<colindex> 99 |> Result.defaultValue m

    Assert.Equal(Ok(Some 99), get actual 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(Some 7), get m 0UL<rowindex> 0UL<colindex>)

[<Fact>]
let ``matrix set inserts new`` () =
    let m =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 7) ]))

    let actual = set m 2UL<rowindex> 2UL<colindex> 42 |> Result.defaultValue m

    Assert.Equal(Ok(Some 42), get actual 2UL<rowindex> 2UL<colindex>)

[<Fact>]
let ``matrix set out of bounds`` () =
    let m = fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, []))
    Assert.Equal(Error Error.InconsistentSizeOfArguments, set m 5UL<rowindex> 5UL<colindex> 99)

[<Fact>]
let ``matrix set then get roundtrip`` () =
    let m0 = empty 4UL<nrows> 4UL<ncols>
    let m1 = set m0 0UL<rowindex> 0UL<colindex> 1 |> Result.defaultValue m0
    let m2 = set m1 1UL<rowindex> 2UL<colindex> 2 |> Result.defaultValue m1
    let m3 = set m2 3UL<rowindex> 3UL<colindex> 3 |> Result.defaultValue m2

    Assert.Equal(Ok(Some 1), get m3 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(Some 2), get m3 1UL<rowindex> 2UL<colindex>)
    Assert.Equal(Ok(Some 3), get m3 3UL<rowindex> 3UL<colindex>)
    Assert.Equal(Ok(None), get m3 2UL<rowindex> 1UL<colindex>)

[<Fact>]
let ``matrix map doubles values`` () =
    let m =
        fromCoordinateListUnchecked (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 3); (1UL<rowindex>, 2UL<colindex>, 5) ]
            )
        )

    let result = map m (Option.map (fun v -> v * 2))

    Assert.Equal(Ok(Some 6), get result 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(Some 10), get result 1UL<rowindex> 2UL<colindex>)
    Assert.Equal(Ok(None), get result 0UL<rowindex> 1UL<colindex>)

[<Fact>]
let ``matrix map filters Some to None`` () =
    let m =
        fromCoordinateListUnchecked (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 3); (1UL<rowindex>, 2UL<colindex>, 5) ]
            )
        )

    let result =
        map m (fun v ->
            match v with
            | Some x when x > 4 -> Some x
            | _ -> None)

    Assert.Equal(Ok(None), get result 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(Some 5), get result 1UL<rowindex> 2UL<colindex>)

[<Fact>]
let ``matrix map fills None with values`` () =
    let m =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ]))

    let result =
        map m (fun v ->
            Some(
                match v with
                | Some x -> x
                | None -> 0
            ))

    Assert.Equal(Ok(Some 3), get result 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(Some 0), get result 1UL<rowindex> 1UL<colindex>)
    Assert.Equal(Ok(Some 0), get result 3UL<rowindex> 3UL<colindex>)

[<Fact>]
let ``matrix map on empty matrix`` () =
    let m = empty 4UL<nrows> 4UL<ncols>

    let result = map m (Option.map (fun v -> v + 1))

    Assert.Equal(Ok(None), get result 0UL<rowindex> 0UL<colindex>)

[<Fact>]
let ``matrix map nvals updated`` () =
    let m =
        fromCoordinateListUnchecked (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 3); (1UL<rowindex>, 2UL<colindex>, 5) ]
            )
        )

    Assert.Equal(2UL, uint64 m.nvals)

    let result = map m (fun _ -> None)

    Assert.Equal(0UL, uint64 result.nvals)

[<Fact>]
let ``matrix mapValues applies only to stored values`` () =
    let m =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ]))

    let result = mapValues m (fun v -> Some(v * 2))

    Assert.Equal(Ok(Some 6), get result 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(None), get result 2UL<rowindex> 2UL<colindex>)
    Assert.Equal(Ok(None), get result 0UL<rowindex> 1UL<colindex>)
    Assert.Equal(1UL, uint64 result.nvals)

[<Fact>]
let ``matrix mapValues can drop stored values`` () =
    let m =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ]))

    let result = mapValues m (fun _ -> None)

    Assert.Equal(0UL, uint64 result.nvals)

[<Fact>]
let ``matrix mapiValues applies only to stored values with indices`` () =
    let m =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (1UL<rowindex>, 2UL<colindex>, 5) ]))

    let result = mapiValues m (fun i j v -> Some(v + int (uint64 i) + int (uint64 j)))

    Assert.Equal(Ok(Some 8), get result 1UL<rowindex> 2UL<colindex>)
    Assert.Equal(Ok(None), get result 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(1UL, uint64 result.nvals)

[<Fact>]
let ``matrix mapi expands uniform leaf`` () =
    let m =
        SparseMatrix(
            4UL<nrows>,
            4UL<ncols>,
            4UL<nvals>,
            Storage(4UL<storageSize>, Matrix.qtree.Node(leaf_v 5, leaf_n (), leaf_n (), leaf_n ()))
        )

    let result =
        mapi m (fun i j v -> v |> Option.map (fun x -> x + int (uint64 i) + int (uint64 j)))

    Assert.Equal(4UL, uint64 result.nvals)
    Assert.Equal(Ok(Some 5), get result 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(Some 6), get result 0UL<rowindex> 1UL<colindex>)
    Assert.Equal(Ok(Some 6), get result 1UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(Some 7), get result 1UL<rowindex> 1UL<colindex>)

[<Fact>]
let ``matrix map2Values applies only where both values present`` () =
    let m1 =
        fromCoordinateListUnchecked (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
            )
        )

    let m2 =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 10) ]))

    match map2Values m1 m2 (fun a b -> Some(a + b + 100)) with
    | Error e -> failwithf "unexpected error %A" e
    | Ok result ->
        Assert.Equal(Ok(Some 111), get result 0UL<rowindex> 0UL<colindex>)
        Assert.Equal(Ok(None), get result 1UL<rowindex> 1UL<colindex>)
        Assert.Equal(1UL, uint64 result.nvals)

[<Fact>]
let ``matrix map2AllCells equals map2`` () =
    let m1 =
        fromCoordinateListUnchecked (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
            )
        )

    let m2 =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 10) ]))

    let f a b =
        match a, b with
        | Some x, Some y -> Some(x + y)
        | _ -> None

    Assert.Equal(map2 m1 m2 f, map2AllCells m1 m2 f)

[<Fact>]
let ``matrix map2AtLeastOne distinguishes both left right`` () =
    let m1 =
        fromCoordinateListUnchecked (
            CoordinateList(
                3UL<nrows>,
                3UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
            )
        )

    let m2 =
        fromCoordinateListUnchecked (
            CoordinateList(
                3UL<nrows>,
                3UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10); (2UL<rowindex>, 2UL<colindex>, 30) ]
            )
        )

    let f =
        function
        | AtLeastOne.Both(a, b) -> Some(a + b)
        | AtLeastOne.Left a -> Some(a * 100)
        | AtLeastOne.Right b -> Some(b * -1)

    match map2AtLeastOne m1 m2 f with
    | Error e -> failwithf "unexpected error %A" e
    | Ok result ->
        Assert.Equal(Ok(Some 11), get result 0UL<rowindex> 0UL<colindex>)
        Assert.Equal(Ok(Some 200), get result 1UL<rowindex> 1UL<colindex>)
        Assert.Equal(Ok(Some -30), get result 2UL<rowindex> 2UL<colindex>)

[<Fact>]
let ``matrix map2LeftValues applies where left value present`` () =
    let m1 =
        fromCoordinateListUnchecked (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
            )
        )

    let m2 =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 10) ]))

    match map2LeftValues m1 m2 (fun a b -> Some(a + (defaultArg b 0))) with
    | Error e -> failwithf "unexpected error %A" e
    | Ok result ->
        Assert.Equal(Ok(Some 11), get result 0UL<rowindex> 0UL<colindex>)
        Assert.Equal(Ok(Some 2), get result 1UL<rowindex> 1UL<colindex>)
        Assert.Equal(2UL, uint64 result.nvals)

[<Fact>]
let ``matrix map2i expands uniform leaves on both sides`` () =
    let m1 =
        SparseMatrix(
            4UL<nrows>,
            4UL<ncols>,
            4UL<nvals>,
            Storage(4UL<storageSize>, Matrix.qtree.Node(leaf_v 5, leaf_n (), leaf_n (), leaf_n ()))
        )

    let m2 =
        SparseMatrix(
            4UL<nrows>,
            4UL<ncols>,
            4UL<nvals>,
            Storage(4UL<storageSize>, Matrix.qtree.Node(leaf_v 10, leaf_n (), leaf_n (), leaf_n ()))
        )

    let f i j a b =
        match a, b with
        | Some x, Some y -> Some(x + y + int (uint64 i) * 10 + int (uint64 j))
        | _ -> None

    match map2i m1 m2 f with
    | Error e -> failwithf "unexpected error %A" e
    | Ok result ->
        Assert.Equal(4UL, uint64 result.nvals)
        Assert.Equal(Ok(Some 15), get result 0UL<rowindex> 0UL<colindex>)
        Assert.Equal(Ok(Some 16), get result 0UL<rowindex> 1UL<colindex>)
        Assert.Equal(Ok(Some 25), get result 1UL<rowindex> 0UL<colindex>)
        Assert.Equal(Ok(Some 26), get result 1UL<rowindex> 1UL<colindex>)

[<Fact>]
let ``matrix map2iValues applies indexed where both values present`` () =
    let m1 =
        fromCoordinateListUnchecked (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 2) ]))

    let m2 =
        fromCoordinateListUnchecked (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 10); (2UL<rowindex>, 2UL<colindex>, 20) ]
            )
        )

    let f i j a b =
        Some(a + b + int (uint64 i) + int (uint64 j))

    match map2iValues m1 m2 f with
    | Error e -> failwithf "unexpected error %A" e
    | Ok result ->
        Assert.Equal(Ok(Some 14), get result 1UL<rowindex> 1UL<colindex>)
        Assert.Equal(Ok(None), get result 2UL<rowindex> 2UL<colindex>)
        Assert.Equal(1UL, uint64 result.nvals)

[<Fact>]
let ``matrix map2iAtLeastOne passes indices and side`` () =
    let m1 =
        fromCoordinateListUnchecked (CoordinateList(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ]))

    let m2 =
        fromCoordinateListUnchecked (
            CoordinateList(
                2UL<nrows>,
                2UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10); (1UL<rowindex>, 1UL<colindex>, 20) ]
            )
        )

    let f i j =
        function
        | AtLeastOne.Both(a, b) -> Some(a + b + int (uint64 i) + int (uint64 j))
        | AtLeastOne.Left a -> Some(a)
        | AtLeastOne.Right b -> Some(b + int (uint64 i) * 100 + int (uint64 j))

    match map2iAtLeastOne m1 m2 f with
    | Error e -> failwithf "unexpected error %A" e
    | Ok result ->
        Assert.Equal(Ok(Some 11), get result 0UL<rowindex> 0UL<colindex>)
        Assert.Equal(Ok(Some 121), get result 1UL<rowindex> 1UL<colindex>)

[<Fact>]
let ``matrix map2iLeftValues applies indexed where left present`` () =
    let m1 =
        fromCoordinateListUnchecked (CoordinateList(2UL<nrows>, 2UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 2) ]))

    let m2 =
        fromCoordinateListUnchecked (CoordinateList(2UL<nrows>, 2UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 10) ]))

    let f i j a b =
        Some(a + (defaultArg b 0) + int (uint64 i) * 10 + int (uint64 j))

    match map2iLeftValues m1 m2 f with
    | Error e -> failwithf "unexpected error %A" e
    | Ok result ->
        Assert.Equal(Ok(Some 23), get result 1UL<rowindex> 1UL<colindex>)
        Assert.Equal(1UL, uint64 result.nvals)

let ``fromCoordinateListUnchecked with out-of-range coordinates returns Error`` () =
    let coo =
        CoordinateList(6UL<nrows>, 6UL<ncols>, [ (9UL<rowindex>, 9UL<colindex>, 13) ])

    let result = fromCoordinateList coo

    match result with
    | Error _ -> ()
    | Ok _ -> Assert.Fail()

[<Fact>]
let ``fromCoordinateList with zero size returns Error`` () =
    let coo =
        CoordinateList(
            0UL<nrows>,
            0UL<ncols>,
            [ (33UL<rowindex>, 33UL<colindex>, 33); (39UL<rowindex>, 39UL<colindex>, 1) ]
        )

    let result = fromCoordinateList coo

    match result with
    | Error _ -> ()
    | Ok _ -> Assert.Fail()

[<Fact>]
let ``fromCoordinateList with unsorted coordinates works correctly`` () =
    let coo =
        CoordinateList(
            7UL<nrows>,
            7UL<ncols>,
            [ (6UL<rowindex>, 6UL<colindex>, 10)
              (1UL<rowindex>, 2UL<colindex>, 8)
              (1UL<rowindex>, 1UL<colindex>, 100) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok matrix ->
        let result = Matrix.toCoordinateList matrix

        Assert.Equal(
            CoordinateList(
                7UL<nrows>,
                7UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 100)
                  (1UL<rowindex>, 2UL<colindex>, 8)
                  (6UL<rowindex>, 6UL<colindex>, 10) ]
            ),
            result
        )
    | _ -> Assert.Fail()

[<Fact>]
let ``fromCoordinateList with duplicate indices returns the last of them`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 33); (1UL<rowindex>, 1UL<colindex>, 100) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok matrix ->
        let result = Matrix.toCoordinateList matrix
        Assert.Equal(CoordinateList(3UL<nrows>, 3UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 100) ]), result)
    | _ -> Assert.Fail()

[<Fact>]
let ``map works on square matrix`` () =
    let clist =
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 22); (2UL<rowindex>, 3UL<colindex>, 37) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok matrix ->
        let result =
            Matrix.map matrix (fun (x: int option) ->
                match x with
                | Some v -> Some(v + 9)
                | None -> None)

        let coo = Matrix.toCoordinateList result

        Assert.Equal(
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 31); (2UL<rowindex>, 3UL<colindex>, 46) ]
            ),
            coo
        )
    | _ -> Assert.Fail()

[<Fact>]
let ``map works on rectangular matrix`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 21); (4UL<rowindex>, 3UL<colindex>, 36) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok matrix ->
        let result =
            Matrix.map matrix (fun (x: int option) ->
                match x with
                | Some v -> Some(v / 3)
                | None -> None)

        let coo = Matrix.toCoordinateList result

        Assert.Equal(
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 7); (4UL<rowindex>, 3UL<colindex>, 12) ]
            ),
            coo
        )
    | _ -> Assert.Fail()

[<Fact>]
let ``map on empty matrix returns empty matrix`` () =
    match Matrix.fromCoordinateList (CoordinateList(0UL<nrows>, 0UL<ncols>, [])) with
    | Result.Ok matrix ->
        let result =
            Matrix.map matrix (fun (x: int option) ->
                match x with
                | Some v -> Some(v * 5)
                | None -> None)

        let coo = Matrix.toCoordinateList result
        Assert.Equal(CoordinateList(0UL<nrows>, 0UL<ncols>, []), coo)
    | _ -> Assert.Fail()

[<Fact>]
let ``map with function that turns all the elements into zeros`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            7UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 5)
              (4UL<rowindex>, 1UL<colindex>, 17)
              (4UL<rowindex>, 6UL<colindex>, 33) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok matrix ->
        let result = Matrix.map matrix (fun _ -> Some(0))
        let coo = Matrix.toCoordinateList result

        let expectedData =
            [ for i in 0UL .. 4UL do
                  for j in 0UL .. 6UL -> (i * 1UL<rowindex>, j * 1UL<colindex>, 0) ]

        let expected = CoordinateList(5UL<nrows>, 7UL<ncols>, expectedData)
        Assert.Equal(expected, coo)
    | _ -> Assert.Fail()

[<Fact>]
let ``map with function that turns all the elements to None`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            7UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 5)
              (4UL<rowindex>, 1UL<colindex>, 17)
              (4UL<rowindex>, 6UL<colindex>, 33) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok matrix ->
        let result = Matrix.map matrix (fun _ -> None)
        Assert.Equal(CoordinateList(5UL<nrows>, 7UL<ncols>, []), Matrix.toCoordinateList result)
    | _ -> Assert.Fail()

[<Fact>]
let ``map can change type from int to string`` () =
    let clist =
        CoordinateList(
            3UL<nrows>,
            5UL<ncols>,
            [ (2UL<rowindex>, 1UL<colindex>, 17); (2UL<rowindex>, 4UL<colindex>, 33) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok matrix ->
        let result =
            Matrix.map matrix (fun (x: int option) ->
                match x with
                | Some v -> Some(sprintf "str %d" v)
                | None -> None)

        let actual = Matrix.toCoordinateList result

        Assert.Equal(
            CoordinateList(
                3UL<nrows>,
                5UL<ncols>,
                [ (2UL<rowindex>, 1UL<colindex>, "str 17")
                  (2UL<rowindex>, 4UL<colindex>, "str 33") ]
            ),
            actual
        )
    | _ -> Assert.Fail()

[<Fact>]
let ``mapi works with row index and col index on square matrix`` () =
    let clist =
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 12); (2UL<rowindex>, 3UL<colindex>, 13) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok matrix ->
        let result =
            Matrix.mapi matrix (fun i j x ->
                match x with
                | Some v -> Some(v * int i + int j)
                | None -> None)

        Assert.Equal(
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 13); (2UL<rowindex>, 3UL<colindex>, 29) ]
            ),
            Matrix.toCoordinateList result
        )
    | _ -> Assert.Fail()

[<Fact>]
let ``mapi works with row index and col index on rectangular matrix`` () =
    let clist =
        CoordinateList(
            3UL<nrows>,
            5UL<ncols>,
            [ (1UL<rowindex>, 2UL<colindex>, 15); (2UL<rowindex>, 2UL<colindex>, 13) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok matrix ->
        let result =
            Matrix.mapi matrix (fun i j x ->
                match x with
                | Some v -> Some(v * int j + int i)
                | None -> None)

        Assert.Equal(
            CoordinateList(
                3UL<nrows>,
                5UL<ncols>,
                [ (1UL<rowindex>, 2UL<colindex>, 31); (2UL<rowindex>, 2UL<colindex>, 28) ]
            ),
            Matrix.toCoordinateList result
        )
    | _ -> Assert.Fail()

[<Fact>]
let ``mapi on empty matrix returns empty matrix`` () =
    match Matrix.fromCoordinateList (CoordinateList(0UL<nrows>, 0UL<ncols>, [])) with
    | Result.Ok matrix ->
        let result =
            Matrix.mapi matrix (fun i j x ->
                match x with
                | Some v -> Some((v + 1) * 3 + 2 * int j)
                | None -> None)

        Assert.Equal(CoordinateList(0UL<nrows>, 0UL<ncols>, []), Matrix.toCoordinateList result)
    | _ -> Assert.Fail()

[<Fact>]
let ``mapi with special function returns empty matrix`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            7UL<ncols>,
            [ (1UL<rowindex>, 2UL<colindex>, 15); (4UL<rowindex>, 5UL<colindex>, 13) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok matrix ->
        let result =
            Matrix.mapi matrix (fun i j x ->
                match x with
                | Some v when (int i + int j) % 2 = 0 -> Some(v * 2)
                | _ -> None)

        Assert.Equal(CoordinateList(5UL<nrows>, 7UL<ncols>, []), Matrix.toCoordinateList result)
    | _ -> Assert.Fail()

[<Fact>]
let ``mapi works with function does not depend on the indexes`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            7UL<ncols>,
            [ (1UL<rowindex>, 2UL<colindex>, 15); (4UL<rowindex>, 5UL<colindex>, 13) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok matrix ->
        let result =
            Matrix.mapi matrix (fun _ _ x ->
                match x with
                | Some v -> Some(v * 2)
                | None -> None)

        Assert.Equal(
            CoordinateList(
                5UL<nrows>,
                7UL<ncols>,
                [ (1UL<rowindex>, 2UL<colindex>, 30); (4UL<rowindex>, 5UL<colindex>, 26) ]
            ),
            Matrix.toCoordinateList result
        )
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns error when row start is negative`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m -1 4 2 3 with
        | Result.Ok _ -> Assert.Fail()
        | Result.Error msg -> Assert.Equal("Start row should be >= 0", msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns error when row end is negative`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 1 -4 2 3 with
        | Result.Ok _ -> Assert.Fail()
        | Result.Error msg -> Assert.Equal("End row should be >= 0", msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns error when col start is negative`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 1 4 -2 3 with
        | Result.Ok _ -> Assert.Fail()
        | Result.Error msg -> Assert.Equal("Start column should be >= 0", msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns error when col end is negative`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 1 4 2 -3 with
        | Result.Ok _ -> Assert.Fail()
        | Result.Error msg -> Assert.Equal("End column should be >= 0", msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns error when row start is out of range`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 6 4 2 3 with
        | Result.Ok _ -> Assert.Fail()
        | Result.Error msg -> Assert.Equal("Start row is out of matrix length", msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns error when row end is out of range`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 1 10 2 3 with
        | Result.Ok _ -> Assert.Fail()
        | Result.Error msg -> Assert.Equal("End row is out of matrix length", msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns error when col start is out of range`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 1 4 10 3 with
        | Result.Ok _ -> Assert.Fail()
        | Result.Error msg -> Assert.Equal("Start column is out of matrix length", msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns error when col end is out of range`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 1 2 2 10 with
        | Result.Ok _ -> Assert.Fail()
        | Result.Error msg -> Assert.Equal("End column is out of matrix length", msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns error when row end is less than row start`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 2 1 2 3 with
        | Result.Ok _ -> Assert.Fail()
        | Result.Error msg -> Assert.Equal("Start row should be <= end row", msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns error when col end is less than col start`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 1 2 3 2 with
        | Result.Ok _ -> Assert.Fail()
        | Result.Error msg -> Assert.Equal("Start column should be <= end column", msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns correct square submatrix`` () =
    let clist =
        CoordinateList(
            7UL<nrows>,
            7UL<ncols>,
            [ (2UL<rowindex>, 2UL<colindex>, 33); (5UL<rowindex>, 5UL<colindex>, 28) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 1 3 1 3 with
        | Result.Ok res ->
            Assert.Equal(
                CoordinateList(3UL<nrows>, 3UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 33) ]),
                Matrix.toCoordinateList res
            )
        | Result.Error msg -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns correct rectangular submatrix`` () =
    let clist =
        CoordinateList(
            5UL<nrows>,
            9UL<ncols>,
            [ (3UL<rowindex>, 7UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 2 4 5 8 with
        | Result.Ok res ->
            Assert.Equal(
                CoordinateList(3UL<nrows>, 4UL<ncols>, [ (1UL<rowindex>, 2UL<colindex>, 33) ]),
                Matrix.toCoordinateList res
            )
        | Result.Error msg -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns empty matrix`` () =
    let clist =
        CoordinateList(
            7UL<nrows>,
            9UL<ncols>,
            [ (3UL<rowindex>, 3UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 4 6 5 8 with
        | Result.Ok result -> Assert.Equal(CoordinateList(3UL<nrows>, 4UL<ncols>, []), Matrix.toCoordinateList result)
        | Result.Error msg -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns single submatrix`` () =
    let clist =
        CoordinateList(
            7UL<nrows>,
            9UL<ncols>,
            [ (3UL<rowindex>, 3UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 1 1 2 2 with
        | Result.Ok result ->
            Assert.Equal(
                CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 28) ]),
                Matrix.toCoordinateList result
            )
        | Result.Error msg -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns correct submatrix equals to matrix`` () =
    let clist =
        CoordinateList(
            7UL<nrows>,
            9UL<ncols>,
            [ (3UL<rowindex>, 3UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 0 6 0 8 with
        | Result.Ok result ->
            Assert.Equal(
                CoordinateList(
                    7UL<nrows>,
                    9UL<ncols>,
                    [ (1UL<rowindex>, 2UL<colindex>, 28); (3UL<rowindex>, 3UL<colindex>, 33) ]
                ),
                Matrix.toCoordinateList result
            )
        | Result.Error msg -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns correct submatrix when row start of submatrix equals to row start of matrix`` () =
    let clist =
        CoordinateList(
            7UL<nrows>,
            7UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 10)
              (3UL<rowindex>, 3UL<colindex>, 33)
              (6UL<rowindex>, 6UL<colindex>, 6) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 0 4 0 4 with
        | Result.Ok result ->
            Assert.Equal(
                CoordinateList(
                    5UL<nrows>,
                    5UL<ncols>,
                    [ (0UL<rowindex>, 0UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 33) ]
                ),
                Matrix.toCoordinateList result
            )
        | Result.Error msg -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns correct submatrix when row end of submatrix equals to row end of matrix`` () =
    let clist =
        CoordinateList(
            7UL<nrows>,
            7UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 10)
              (3UL<rowindex>, 3UL<colindex>, 33)
              (6UL<rowindex>, 6UL<colindex>, 6) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 2 6 2 4 with
        | Result.Ok result ->
            Assert.Equal(
                CoordinateList(5UL<nrows>, 3UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 33) ]),
                Matrix.toCoordinateList result
            )
        | Result.Error msg -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns correct submatrix when col start of submatrix equals to col start of matrix`` () =
    let clist =
        CoordinateList(
            7UL<nrows>,
            7UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 10)
              (3UL<rowindex>, 3UL<colindex>, 33)
              (6UL<rowindex>, 6UL<colindex>, 6) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 2 5 0 6 with
        | Result.Ok result ->
            Assert.Equal(
                CoordinateList(4UL<nrows>, 7UL<ncols>, [ (1UL<rowindex>, 3UL<colindex>, 33) ]),
                Matrix.toCoordinateList result
            )
        | Result.Error msg -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns correct submatrix when col end of submatrix equals to col end of matrix`` () =
    let clist =
        CoordinateList(
            7UL<nrows>,
            7UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 10)
              (3UL<rowindex>, 3UL<colindex>, 33)
              (6UL<rowindex>, 6UL<colindex>, 6) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 2 4 2 6 with
        | Result.Ok result ->
            Assert.Equal(
                CoordinateList(3UL<nrows>, 5UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 33) ]),
                Matrix.toCoordinateList result
            )
        | Result.Error msg -> Assert.Fail()
    | _ -> Assert.Fail()

[<Fact>]
let ``slice returns single column`` () =
    let clist =
        CoordinateList(
            10UL<nrows>,
            10UL<ncols>,
            [ (1UL<rowindex>, 3UL<colindex>, 1)
              (2UL<rowindex>, 2UL<colindex>, 2)
              (5UL<rowindex>, 7UL<colindex>, 3) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 1 6 2 2 with
        | Result.Ok res ->
            Assert.Equal(
                CoordinateList(6UL<nrows>, 1UL<ncols>, [ (1UL<rowindex>, 0UL<colindex>, 2) ]),
                Matrix.toCoordinateList res
            )
        | Result.Error msg -> Assert.Fail(msg)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns single row`` () =
    let clist =
        CoordinateList(
            10UL<nrows>,
            10UL<ncols>,
            [ (1UL<rowindex>, 3UL<colindex>, 1)
              (2UL<rowindex>, 2UL<colindex>, 2)
              (5UL<rowindex>, 7UL<colindex>, 3) ]
        )

    match Matrix.fromCoordinateList clist with
    | Result.Ok m ->
        match Matrix.slice m 5 5 3 9 with
        | Result.Ok res ->
            Assert.Equal(
                CoordinateList(1UL<nrows>, 7UL<ncols>, [ (0UL<rowindex>, 4UL<colindex>, 3) ]),
                Matrix.toCoordinateList res
            )
        | Result.Error msg -> Assert.Fail(msg)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice middle of dense matrix returns correct values`` () =
    let nrows = 64UL<nrows>
    let ncols = 64UL<ncols>

    let coords =
        [ for r in 0UL .. 63UL do
              for c in 0UL .. 63UL do
                  (r * 1UL<rowindex>, c * 1UL<colindex>, 1.0) ]

    match Matrix.fromCoordinateList (Matrix.CoordinateList(nrows, ncols, coords)) with
    | Error msg -> Assert.Fail($"Failed to create dense matrix: {msg}")
    | Ok m ->
        match Matrix.slice m 16 47 16 47 with
        | Error msg -> Assert.Fail($"Slice failed: {msg}")
        | Ok sliced ->
            Assert.Equal(32UL<nrows>, sliced.nrows)
            Assert.Equal(32UL<ncols>, sliced.ncols)

            let coo = Matrix.toCoordinateList sliced
            let elements = coo.list

            Assert.Equal(1024, List.length elements)

            for (_, _, v) in elements do
                Assert.Equal(1.0, v)

            Assert.Equal(1024UL<nvals>, sliced.nvals)

[<Fact>]
let ``reduceRows op_add on square power of two matrix`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 12)
              (0UL<rowindex>, 1UL<colindex>, 15)
              (1UL<rowindex>, 0UL<colindex>, 17)
              (1UL<rowindex>, 1UL<colindex>, 3) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Error msg -> Assert.Fail()
    | Result.Ok matrix ->

        let result = Matrix.reduceRows op_add matrix

        let vectorCoordinates = Vector.toCoordinateList result

        let expected =
            Vector.CoordinateList(2UL<Vector.dataLength>, [ (0UL<Vector.index>, 27); (1UL<Vector.index>, 20) ])

        Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``reduceRows op_add on square power of two matrix with empty row`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (1UL<rowindex>, 0UL<colindex>, 17); (1UL<rowindex>, 1UL<colindex>, 3) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok m ->

        let res = Matrix.reduceRows op_add m

        Assert.Equal(
            Vector.CoordinateList(2UL<Vector.dataLength>, [ (1UL<Vector.index>, 20) ]),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceRows op_add on square not power of two matrix`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13)
              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok m ->

        let res = Matrix.reduceRows op_add m

        Assert.Equal(
            Vector.CoordinateList(
                3UL<Vector.dataLength>,
                [ (0UL<Vector.index>, 21); (1UL<Vector.index>, 24); (2UL<Vector.index>, 32) ]
            ),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceRows op_mult on square not power of two matrix`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13)
              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok m ->

        let res = Matrix.reduceRows op_mult m

        Assert.Equal(
            Vector.CoordinateList(
                3UL<Vector.dataLength>,
                [ (0UL<Vector.index>, 315); (1UL<Vector.index>, 143); (2UL<Vector.index>, 255) ]
            ),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceRows op_add on rectangular matrix`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok m ->

        let res = Matrix.reduceRows op_add m

        Assert.Equal(
            Vector.CoordinateList(2UL<Vector.dataLength>, [ (0UL<Vector.index>, 21); (1UL<Vector.index>, 24) ]),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceRows op_add on empty matrix`` () =
    match Matrix.fromCoordinateList (CoordinateList(2UL<nrows>, 3UL<ncols>, [])) with
    | Result.Ok m ->

        let res = Matrix.reduceRows op_add m
        Assert.Equal(Vector.CoordinateList(2UL<Vector.dataLength>, []), Vector.toCoordinateList res)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceRows op_mult on single matrix`` () =
    match
        Matrix.fromCoordinateList (CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 33) ]))
    with
    | Result.Ok m ->

        let res = Matrix.reduceRows op_mult m

        Assert.Equal(
            Vector.CoordinateList(1UL<Vector.dataLength>, [ (0UL<Vector.index>, 33) ]),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceCols op_add on square power of two matrix`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 12)
              (0UL<rowindex>, 1UL<colindex>, 15)
              (1UL<rowindex>, 0UL<colindex>, 17)
              (1UL<rowindex>, 1UL<colindex>, 3) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok m ->

        let res = Matrix.reduceCols op_add m

        Assert.Equal(
            Vector.CoordinateList(2UL<Vector.dataLength>, [ (0UL<Vector.index>, 29); (1UL<Vector.index>, 18) ]),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceCols op_add on square power of two matrix with empty col`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 17); (1UL<rowindex>, 0UL<colindex>, 3) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok m ->

        let res = Matrix.reduceCols op_add m

        Assert.Equal(
            Vector.CoordinateList(2UL<Vector.dataLength>, [ (0UL<Vector.index>, 20) ]),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceCols op_add on square not power of two matrix`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13)
              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok m ->

        let res = Matrix.reduceCols op_add m

        Assert.Equal(
            Vector.CoordinateList(
                3UL<Vector.dataLength>,
                [ (0UL<Vector.index>, 20); (1UL<Vector.index>, 35); (2UL<Vector.index>, 22) ]
            ),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceCols op_mult on square not power of two matrix`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13)
              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 17) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok m ->

        let res = Matrix.reduceCols op_mult m

        Assert.Equal(
            Vector.CoordinateList(
                3UL<Vector.dataLength>,
                [ (0UL<Vector.index>, 75); (1UL<Vector.index>, 1309); (2UL<Vector.index>, 117) ]
            ),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceCols op_add on rectangular matrix`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13) ]
        )

    match Matrix.fromCoordinateList coo with
    | Result.Ok m ->

        let res = Matrix.reduceCols op_add m

        Assert.Equal(
            Vector.CoordinateList(
                3UL<Vector.dataLength>,
                [ (0UL<Vector.index>, 5); (1UL<Vector.index>, 18); (2UL<Vector.index>, 22) ]
            ),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceCols op_add on empty matrix`` () =
    match Matrix.fromCoordinateList (CoordinateList(2UL<nrows>, 3UL<ncols>, [])) with
    | Result.Ok m ->

        let res = Matrix.reduceCols op_add m
        Assert.Equal(Vector.CoordinateList(3UL<Vector.dataLength>, []), Vector.toCoordinateList res)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``reduceCols op_mult on single matrix`` () =
    match
        Matrix.fromCoordinateList (CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 33) ]))
    with
    | Result.Ok m ->

        let res = Matrix.reduceCols op_mult m

        Assert.Equal(
            Vector.CoordinateList(1UL<Vector.dataLength>, [ (0UL<Vector.index>, 33) ]),
            Vector.toCoordinateList res
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``kronecker product with square power of two x square power of two matrixes`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let cooB =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 6)
              (1UL<rowindex>, 0UL<colindex>, 7)
              (1UL<rowindex>, 1UL<colindex>, 8) ]
        )

    let expected =
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 6)
              (0UL<rowindex>, 2UL<colindex>, 10)
              (0UL<rowindex>, 3UL<colindex>, 12)
              (1UL<rowindex>, 0UL<colindex>, 7)
              (1UL<rowindex>, 1UL<colindex>, 8)
              (1UL<rowindex>, 2UL<colindex>, 14)
              (1UL<rowindex>, 3UL<colindex>, 16)
              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 18)
              (2UL<rowindex>, 2UL<colindex>, 20)
              (2UL<rowindex>, 3UL<colindex>, 24)
              (3UL<rowindex>, 0UL<colindex>, 21)
              (3UL<rowindex>, 1UL<colindex>, 24)
              (3UL<rowindex>, 2UL<colindex>, 28)
              (3UL<rowindex>, 3UL<colindex>, 32) ]
        )

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooB with
    | Result.Ok a, Result.Ok b ->
        match Matrix.kroneckerProduct a b (fun a b -> Some(a * b)) with
        | Result.Ok res ->
            let actual = Matrix.toCoordinateList res
            Assert.Equal(expected, actual)
        | Result.Error msg -> Assert.Fail(msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker product with square not power of two x square not power of two matrixes`` () =
    let cooA =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (0UL<rowindex>, 2UL<colindex>, 3)
              (1UL<rowindex>, 0UL<colindex>, 4)
              (1UL<rowindex>, 1UL<colindex>, 5)
              (1UL<rowindex>, 2UL<colindex>, 6)
              (2UL<rowindex>, 0UL<colindex>, 7)
              (2UL<rowindex>, 1UL<colindex>, 8)
              (2UL<rowindex>, 2UL<colindex>, 9) ]
        )

    let cooB =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 10)
              (0UL<rowindex>, 1UL<colindex>, 11)
              (0UL<rowindex>, 2UL<colindex>, 12)
              (1UL<rowindex>, 0UL<colindex>, 13)
              (1UL<rowindex>, 1UL<colindex>, 14)
              (1UL<rowindex>, 2UL<colindex>, 15)
              (2UL<rowindex>, 0UL<colindex>, 16)
              (2UL<rowindex>, 1UL<colindex>, 17)
              (2UL<rowindex>, 2UL<colindex>, 18) ]
        )

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooB with
    | Result.Ok a, Result.Ok b ->
        match Matrix.kroneckerProduct a b (fun a b -> Some(a * b)) with
        | Result.Ok res -> Assert.Equal(9UL<nrows>, (Matrix.toCoordinateList res).nrows)
        | Result.Error msg -> Assert.Fail(msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker product with rectangular and square matrixes`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 2UL<colindex>, 4) ]
        )

    let cooB =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 6)
              (1UL<rowindex>, 0UL<colindex>, 7)
              (1UL<rowindex>, 1UL<colindex>, 8) ]
        )

    let expected =
        CoordinateList(
            4UL<nrows>,
            6UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 6)
              (0UL<rowindex>, 2UL<colindex>, 10)
              (0UL<rowindex>, 3UL<colindex>, 12)
              (1UL<rowindex>, 0UL<colindex>, 7)
              (1UL<rowindex>, 1UL<colindex>, 8)
              (1UL<rowindex>, 2UL<colindex>, 14)
              (1UL<rowindex>, 3UL<colindex>, 16)
              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 18)
              (2UL<rowindex>, 4UL<colindex>, 20)
              (2UL<rowindex>, 5UL<colindex>, 24)
              (3UL<rowindex>, 0UL<colindex>, 21)
              (3UL<rowindex>, 1UL<colindex>, 24)
              (3UL<rowindex>, 4UL<colindex>, 28)
              (3UL<rowindex>, 5UL<colindex>, 32) ]
        )

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooB with
    | Result.Ok a, Result.Ok b ->
        match Matrix.kroneckerProduct a b (fun a b -> Some(a * b)) with
        | Result.Ok res ->
            let actual = Matrix.toCoordinateList res
            Assert.Equal(expected, actual)
        | Result.Error msg -> Assert.Fail msg
    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker product with square and rectangular matrixes`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let cooB =
        CoordinateList(
            2UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 6)
              (0UL<rowindex>, 2UL<colindex>, 7)
              (1UL<rowindex>, 0UL<colindex>, 8)
              (1UL<rowindex>, 1UL<colindex>, 9)
              (1UL<rowindex>, 2UL<colindex>, 10) ]
        )

    let expectedElements =
        [ (0UL<rowindex>, 0UL<colindex>, 5)
          (0UL<rowindex>, 1UL<colindex>, 6)
          (0UL<rowindex>, 2UL<colindex>, 7)

          (1UL<rowindex>, 0UL<colindex>, 8)
          (1UL<rowindex>, 1UL<colindex>, 9)
          (1UL<rowindex>, 2UL<colindex>, 10)

          (0UL<rowindex>, 3UL<colindex>, 10)
          (0UL<rowindex>, 4UL<colindex>, 12)
          (0UL<rowindex>, 5UL<colindex>, 14)

          (1UL<rowindex>, 3UL<colindex>, 16)
          (1UL<rowindex>, 4UL<colindex>, 18)
          (1UL<rowindex>, 5UL<colindex>, 20)

          (2UL<rowindex>, 0UL<colindex>, 15)
          (2UL<rowindex>, 1UL<colindex>, 18)
          (2UL<rowindex>, 2UL<colindex>, 21)

          (3UL<rowindex>, 0UL<colindex>, 24)
          (3UL<rowindex>, 1UL<colindex>, 27)
          (3UL<rowindex>, 2UL<colindex>, 30)

          (2UL<rowindex>, 3UL<colindex>, 20)
          (2UL<rowindex>, 4UL<colindex>, 24)
          (2UL<rowindex>, 5UL<colindex>, 28)

          (3UL<rowindex>, 3UL<colindex>, 32)
          (3UL<rowindex>, 4UL<colindex>, 36)
          (3UL<rowindex>, 5UL<colindex>, 40) ]
        |> List.sortBy (fun (r, c, _) -> (r, c))

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooB with
    | Result.Ok a, Result.Ok b ->
        match Matrix.kroneckerProduct a b (fun a b -> Some(a * b)) with
        | Result.Ok res ->
            let coo = Matrix.toCoordinateList res

            Assert.Equal(4UL<nrows>, coo.nrows)
            Assert.Equal(6UL<ncols>, coo.ncols)

            Assert.Equal<seq<uint64<rowindex> * uint64<colindex> * int>>(
                expectedElements,
                coo.list |> List.sortBy (fun (r, c, _) -> (r, c))
            )

        | Result.Error msg -> Assert.Fail(msg)

    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker product of matrix with empty matrix`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let cooEmpty = CoordinateList(2UL<nrows>, 2UL<ncols>, [])

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooEmpty with
    | Result.Ok a, Result.Ok empty ->
        match Matrix.kroneckerProduct a empty (fun a b -> Some(a * b)) with
        | Result.Ok res -> Assert.Equal(CoordinateList(4UL<nrows>, 4UL<ncols>, []), Matrix.toCoordinateList res)
        | Result.Error msg -> Assert.Fail(msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker product of empty matrix with matrix`` () =
    let cooEmpty = CoordinateList(2UL<nrows>, 2UL<ncols>, [])

    let cooB =
        CoordinateList(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 4) ])

    match Matrix.fromCoordinateList cooEmpty, Matrix.fromCoordinateList cooB with
    | Result.Ok empty, Result.Ok b ->
        match Matrix.kroneckerProduct empty b (fun a b -> Some(a * b)) with
        | Result.Ok res -> Assert.Equal(CoordinateList(4UL<nrows>, 4UL<ncols>, []), Matrix.toCoordinateList res)
        | Result.Error msg -> Assert.Fail(msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker product of matrix with zeros`` () =
    let cooA =
        CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 2) ])

    let cooB =
        CoordinateList(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 0); (1UL<rowindex>, 1UL<colindex>, 3) ])

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooB with
    | Result.Ok a, Result.Ok b ->
        match Matrix.kroneckerProduct a b (fun a b -> Some(a * b)) with
        | Result.Ok res ->
            let expected =
                [ (0UL<rowindex>, 0UL<colindex>, 0); (1UL<rowindex>, 1UL<colindex>, 6) ]

            let actual =
                (Matrix.toCoordinateList res).list |> List.sortBy (fun (r, c, _) -> (r, c))

            Assert.Equal<(uint64<rowindex> * uint64<colindex> * int) list>(expected, actual)
        | Result.Error msg -> Assert.Fail(msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker product resulting entirely in explicit zeros`` () =
    let cooA =
        CoordinateList(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 0); (1UL<rowindex>, 1UL<colindex>, 0) ])

    let cooB =
        CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 5) ])

    match Matrix.fromCoordinateList cooA with
    | Result.Error msg -> Assert.Fail(msg)
    | Result.Ok a ->
        match Matrix.fromCoordinateList cooB with
        | Result.Error msg -> Assert.Fail(msg)
        | Result.Ok b ->
            match Matrix.kroneckerProduct a b (fun a b -> Some(a * b)) with
            | Result.Ok res ->
                let coo = Matrix.toCoordinateList res

                let expectedElements =
                    [ (0UL<rowindex>, 0UL<colindex>, 0); (1UL<rowindex>, 1UL<colindex>, 0) ]
                    |> List.sortBy (fun (r, c, _) -> (r, c))

                Assert.Equal(2UL<nrows>, coo.nrows)
                Assert.Equal(2UL<ncols>, coo.ncols)

                Assert.Equal<seq<uint64<rowindex> * uint64<colindex> * int>>(
                    expectedElements,
                    coo.list |> List.sortBy (fun (r, c, _) -> (r, c))
                )
            | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``kronecker product of square matrix with matrix 1x1`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let cooB =
        CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ])

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooB with
    | Result.Ok a, Result.Ok b ->
        match Matrix.kroneckerProduct a b (fun a b -> Some(a * b)) with
        | Result.Ok res ->
            let expected =
                CoordinateList(
                    2UL<nrows>,
                    2UL<ncols>,
                    [ (0UL<rowindex>, 0UL<colindex>, 3)
                      (0UL<rowindex>, 1UL<colindex>, 6)
                      (1UL<rowindex>, 0UL<colindex>, 9)
                      (1UL<rowindex>, 1UL<colindex>, 12) ]
                )

            Assert.Equal(expected, Matrix.toCoordinateList res)
        | Result.Error msg -> Assert.Fail(msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker product of matrix 1x1 with square matrix`` () =
    let cooA =
        CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ])

    let cooB =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooB with
    | Result.Ok a, Result.Ok b ->
        match Matrix.kroneckerProduct a b (fun a b -> Some(a * b)) with
        | Result.Ok res ->
            let expected =
                CoordinateList(
                    2UL<nrows>,
                    2UL<ncols>,
                    [ (0UL<rowindex>, 0UL<colindex>, 3)
                      (0UL<rowindex>, 1UL<colindex>, 6)
                      (1UL<rowindex>, 0UL<colindex>, 9)
                      (1UL<rowindex>, 1UL<colindex>, 12) ]
                )

            Assert.Equal(expected, Matrix.toCoordinateList res)
        | Result.Error msg -> Assert.Fail(msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker dimension check`` () =
    let cooA =
        CoordinateList(3UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    let cooB =
        CoordinateList(2UL<nrows>, 5UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooB with
    | Result.Ok a, Result.Ok b ->
        match Matrix.kroneckerProduct a b (fun a b -> Some(a * b)) with
        | Result.Ok res ->
            Assert.Equal(6UL<nrows>, res.nrows)
            Assert.Equal(20UL<ncols>, res.ncols)
        | Result.Error msg -> Assert.Fail(msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker product with sparse matrix on dense matrix`` () =
    let cooA =
        CoordinateList(10UL<nrows>, 10UL<ncols>, [ (5UL<rowindex>, 5UL<colindex>, 2) ])

    let cooB =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (0UL<rowindex>, 2UL<colindex>, 3)
              (1UL<rowindex>, 0UL<colindex>, 4)
              (1UL<rowindex>, 1UL<colindex>, 5)
              (1UL<rowindex>, 2UL<colindex>, 6)
              (2UL<rowindex>, 0UL<colindex>, 7)
              (2UL<rowindex>, 1UL<colindex>, 8)
              (2UL<rowindex>, 2UL<colindex>, 9) ]
        )

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooB with
    | Result.Ok a, Result.Ok b ->
        match Matrix.kroneckerProduct a b (fun a b -> Some(a * b)) with
        | Result.Ok res ->
            let expected =
                CoordinateList(
                    30UL<nrows>,
                    30UL<ncols>,
                    [ (15UL<rowindex>, 15UL<colindex>, 2)
                      (15UL<rowindex>, 16UL<colindex>, 4)
                      (15UL<rowindex>, 17UL<colindex>, 6)
                      (16UL<rowindex>, 15UL<colindex>, 8)
                      (16UL<rowindex>, 16UL<colindex>, 10)
                      (16UL<rowindex>, 17UL<colindex>, 12)
                      (17UL<rowindex>, 15UL<colindex>, 14)
                      (17UL<rowindex>, 16UL<colindex>, 16)
                      (17UL<rowindex>, 17UL<colindex>, 18) ]
                )

            Assert.Equal(expected, Matrix.toCoordinateList res)
        | Result.Error msg -> Assert.Fail(msg)
    | _ -> Assert.Fail()

[<Fact>]
let ``kronecker product with filtering (only even results)`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let cooB =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    match Matrix.fromCoordinateList cooA, Matrix.fromCoordinateList cooB with
    | Result.Ok a, Result.Ok b ->
        let evenOnly a b =
            let prod = a * b in if prod % 2 = 0 then Some prod else None

        match Matrix.kroneckerProduct a b evenOnly with
        | Result.Ok res ->
            let coo = Matrix.toCoordinateList res
            Assert.True(coo.list |> List.forall (fun (_, _, v) -> v % 2 = 0))
            Assert.True(List.length coo.list < 16)
        | Result.Error msg -> Assert.Fail(msg)
    | _ -> Assert.Fail()
