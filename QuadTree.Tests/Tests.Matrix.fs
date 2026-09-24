module Matrix.Tests

open System
open Xunit

open Matrix
open COO
open Common

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

let op_add x y =
    match (x, y) with
    | Some(a), Some(b) -> Some(a + b)
    | Some(a), _
    | _, Some(a) -> Some(a)
    | _ -> None

let op_mult x y =
    match (x, y) with
    | Some(a), Some(b) -> Some(a * b)
    | _ -> None
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
    let m1 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1)
                  (0UL<rowindex>, 1UL<colindex>, 2)
                  (1UL<rowindex>, 0UL<colindex>, 3)
                  (1UL<rowindex>, 1UL<colindex>, 4) ]
            )
        )

    let m2 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10)
                  (0UL<rowindex>, 1UL<colindex>, 20)
                  (1UL<rowindex>, 0UL<colindex>, 30)
                  (1UL<rowindex>, 1UL<colindex>, 40) ]
            )
        )

    let f row col x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b + int row + int col)
        | _ -> None

    let expected =
        Matrix.CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 11)
              (0UL<rowindex>, 1UL<colindex>, 23)
              (1UL<rowindex>, 0UL<colindex>, 34)
              (1UL<rowindex>, 1UL<colindex>, 46) ]
        )
        |> Matrix.fromCoordinateList
        |> Ok

    let actual = Matrix.map2i m1 m2 f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Matrix.map2i. Square where number of cols and rows are not power of two.`` () =
    let m1 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                3UL<nrows>,
                3UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1)
                  (0UL<rowindex>, 1UL<colindex>, 2)
                  (0UL<rowindex>, 2UL<colindex>, 3)
                  (1UL<rowindex>, 0UL<colindex>, 4)
                  (1UL<rowindex>, 1UL<colindex>, 5)
                  (1UL<rowindex>, 2UL<colindex>, 6) ]
            )
        )

    let m2 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                3UL<nrows>,
                3UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10)
                  (0UL<rowindex>, 1UL<colindex>, 10)
                  (0UL<rowindex>, 2UL<colindex>, 10)
                  (1UL<rowindex>, 0UL<colindex>, 10)
                  (1UL<rowindex>, 1UL<colindex>, 10)
                  (1UL<rowindex>, 2UL<colindex>, 10) ]
            )
        )

    let f row col x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a * (int row + 1) + b * (int col + 1))
        | _ -> None

    let actual = Matrix.map2i m1 m2 f

    let expected =
        Matrix.CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 11)
              (0UL<rowindex>, 1UL<colindex>, 22)
              (0UL<rowindex>, 2UL<colindex>, 33)
              (1UL<rowindex>, 0UL<colindex>, 18)
              (1UL<rowindex>, 1UL<colindex>, 30)
              (1UL<rowindex>, 2UL<colindex>, 42) ]
        )
        |> Matrix.fromCoordinateList
        |> Ok

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Matrix.map2i. Mixed values.`` () =
    let m1 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 3) ]
            )
        )

    let m2 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 30) ]
            )
        )

    let f row col x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | Some(a), None -> Some(int col + a * 2)
        | None, Some(b) -> Some(int row + b * 3)
        | _ -> None

    let actual = Matrix.map2i m1 m2 f

    let expected =
        Matrix.CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 2)
              (1UL<rowindex>, 1UL<colindex>, 31)
              (2UL<rowindex>, 2UL<colindex>, 8)
              (3UL<rowindex>, 3UL<colindex>, 93) ]
        )
        |> Matrix.fromCoordinateList
        |> Ok

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Matrix.mapi. Square where number of cols and rows are power of two.`` () =
    let m =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1)
                  (0UL<rowindex>, 1UL<colindex>, 2)
                  (1UL<rowindex>, 0UL<colindex>, 3)
                  (1UL<rowindex>, 1UL<colindex>, 4) ]
            )
        )

    let f row col x =
        match x with
        | Some(a) -> Some(a + int row + int col)
        | _ -> None

    let actual = Matrix.mapi m f
    let actualCL = Matrix.toCoordinateList actual

    Assert.Equal(4UL<nvals>, actual.nvals)

[<Fact>]
let ``Simple Matrix.mapi. Square where number of cols and rows are not power of two.`` () =
    let m =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                3UL<nrows>,
                3UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1)
                  (0UL<rowindex>, 1UL<colindex>, 2)
                  (0UL<rowindex>, 2UL<colindex>, 3)
                  (1UL<rowindex>, 0UL<colindex>, 4)
                  (1UL<rowindex>, 1UL<colindex>, 5)
                  (1UL<rowindex>, 2UL<colindex>, 6) ]
            )
        )

    let f row col x =
        match x with
        | Some(a) -> Some(a * (int row + 1) * (int col + 1))
        | _ -> None

    let actual = Matrix.mapi m f
    let actualCL = Matrix.toCoordinateList actual

    Assert.Equal(6UL<nvals>, actual.nvals)

[<Fact>]
let ``Simple Matrix.mapi. Multiply row index by value.`` () =
    let m =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1)
                  (1UL<rowindex>, 1UL<colindex>, 2)
                  (2UL<rowindex>, 2UL<colindex>, 3)
                  (3UL<rowindex>, 3UL<colindex>, 4) ]
            )
        )

    let f row col x =
        match x with
        | Some(a) -> Some(a * int row)
        | _ -> None

    let actual = Matrix.mapi m f
    let actualCL = Matrix.toCoordinateList actual

    Assert.Equal(4UL<nvals>, actual.nvals)

[<Fact>]
let ``Conversion identity`` () =
    let id = toCoordinateList << fromCoordinateList

    let nrows = 10UL<nrows>
    let ncols = 12UL<ncols>

    let data =
        [ 0UL<rowindex>, 3UL<colindex>, 10
          3UL<rowindex>, 3UL<colindex>, 33
          9UL<rowindex>, 2UL<colindex>, 5
          3UL<rowindex>, 11UL<colindex>, 1 ]
        |> List.sort

    let coordinates = CoordinateList(nrows, ncols, data)

    let expected = coordinates
    let actual = id coordinates

    Assert.Equal(expected, actual)

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
        let expectedList =
            [ 0UL<rowindex>, 3UL<colindex>, 10
              3UL<rowindex>, 3UL<colindex>, 33
              9UL<rowindex>, 2UL<colindex>, 5
              3UL<rowindex>, 11UL<colindex>, 1 ]
            |> List.sort

        CoordinateList(nrows, ncols, expectedList)

    let actual =
        let c1 = CoordinateList(nrows, ncols, d1)
        let c2 = CoordinateList(nrows, ncols, d2)
        let m1 = fromCoordinateList c1
        let m2 = fromCoordinateList c2

        let addition o1 o2 =
            match o1, o2 with
            | Some x, Some y -> Some(x + y)
            | Some x, None
            | None, Some x -> Some x
            | None, None -> None

        let result =
            match map2 m1 m2 addition with
            | Ok x -> x
            | _ -> failwith "Unreachable"

        toCoordinateList result

    Assert.Equal(expected, actual)

[<Fact>]
let ``Condensation of empty`` () =
    let clist = CoordinateList(2UL<nrows>, 3UL<ncols>, [])

    let actual = fromCoordinateList clist

    // 2 * 3 = 5
    // 4 * 4 None and Dummy
    // NN N D
    // NN N D
    // DDDD
    // DDDD
    let tree =
        qtree.Node(leaf_n (), qtree.Node(leaf_n (), leaf_d (), leaf_n (), leaf_d ()), leaf_d (), leaf_d ())

    let expected =
        SparseMatrix(2UL<nrows>, 3UL<ncols>, 0UL<nvals>, Storage(4UL<storageSize>, tree))

    Assert.Equal(expected.storage.data, actual.storage.data)

[<Fact>]
let ``Condensation of sparse`` () =
    let clist =
        CoordinateList(4UL<nrows>, 3UL<ncols>, [ 0UL<rowindex>, 2UL<colindex>, 2; 3UL<rowindex>, 2UL<colindex>, 4 ])

    let actual = fromCoordinateList clist

    // NN2D
    // NNND
    // NNND
    // NN4D

    let tree =
        qtree.Node(
            leaf_n (),
            qtree.Node(leaf_v 2, leaf_d (), leaf_n (), leaf_d ()),
            leaf_n (),
            qtree.Node(leaf_n (), leaf_d (), leaf_v 4, leaf_d ())
        )

    let expected =
        SparseMatrix(4UL<nrows>, 3UL<ncols>, 0UL<nvals>, Storage(4UL<storageSize>, tree))

    Assert.Equal(expected.storage.data, actual.storage.data)

[<Fact>]
let ``fold -> sum`` () =
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
let ``Fold sum`` () =
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
        fromCoordinateList (
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
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 7) ]))

    Assert.Equal(Ok None, get m 1UL<rowindex> 1UL<colindex>)

[<Fact>]
let ``matrix get out of bounds`` () =
    let m = fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, []))
    Assert.Throws<System.ArgumentOutOfRangeException>(fun () -> get m 5UL<rowindex> 5UL<colindex> |> ignore)

[<Fact>]
let ``matrix set replaces existing`` () =
    let m =
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 7) ]))

    let actual = set m 0UL<rowindex> 0UL<colindex> 99 |> Result.defaultValue m

    Assert.Equal(Ok(Some 99), get actual 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(Some 7), get m 0UL<rowindex> 0UL<colindex>)

[<Fact>]
let ``matrix set inserts new`` () =
    let m =
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 7) ]))

    let actual = set m 2UL<rowindex> 2UL<colindex> 42 |> Result.defaultValue m

    Assert.Equal(Ok(Some 42), get actual 2UL<rowindex> 2UL<colindex>)

[<Fact>]
let ``matrix set out of bounds`` () =
    let m = fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, []))
    Assert.Throws<System.ArgumentOutOfRangeException>(fun () -> set m 5UL<rowindex> 5UL<colindex> 99 |> ignore)

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
        fromCoordinateList (
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
        fromCoordinateList (
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
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ]))

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
        fromCoordinateList (
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
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ]))

    let result = mapValues m (fun v -> Some(v * 2))

    Assert.Equal(Ok(Some 6), get result 0UL<rowindex> 0UL<colindex>)
    Assert.Equal(Ok(None), get result 2UL<rowindex> 2UL<colindex>)
    Assert.Equal(Ok(None), get result 0UL<rowindex> 1UL<colindex>)
    Assert.Equal(1UL, uint64 result.nvals)

[<Fact>]
let ``matrix mapValues can drop stored values`` () =
    let m =
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ]))

    let result = mapValues m (fun _ -> None)

    Assert.Equal(0UL, uint64 result.nvals)

[<Fact>]
let ``matrix mapiValues applies only to stored values with indices`` () =
    let m =
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (1UL<rowindex>, 2UL<colindex>, 5) ]))

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
        fromCoordinateList (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
            )
        )

    let m2 =
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 10) ]))

    match map2Values m1 m2 (fun a b -> Some(a + b + 100)) with
    | Error e -> failwithf "unexpected error %A" e
    | Ok result ->
        Assert.Equal(Ok(Some 111), get result 0UL<rowindex> 0UL<colindex>)
        Assert.Equal(Ok(None), get result 1UL<rowindex> 1UL<colindex>)
        Assert.Equal(1UL, uint64 result.nvals)

[<Fact>]
let ``matrix map2AllCells equals map2`` () =
    let m1 =
        fromCoordinateList (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
            )
        )

    let m2 =
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 10) ]))

    let f a b =
        match a, b with
        | Some x, Some y -> Some(x + y)
        | _ -> None

    Assert.Equal(map2 m1 m2 f, map2AllCells m1 m2 f)

[<Fact>]
let ``matrix map2AtLeastOne distinguishes both left right`` () =
    let m1 =
        fromCoordinateList (
            CoordinateList(
                3UL<nrows>,
                3UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
            )
        )

    let m2 =
        fromCoordinateList (
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
        fromCoordinateList (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
            )
        )

    let m2 =
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 10) ]))

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
        fromCoordinateList (CoordinateList(4UL<nrows>, 4UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 2) ]))

    let m2 =
        fromCoordinateList (
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
        fromCoordinateList (CoordinateList(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ]))

    let m2 =
        fromCoordinateList (
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
        fromCoordinateList (CoordinateList(2UL<nrows>, 2UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 2) ]))

    let m2 =
        fromCoordinateList (CoordinateList(2UL<nrows>, 2UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 10) ]))

    let f i j a b =
        Some(a + (defaultArg b 0) + int (uint64 i) * 10 + int (uint64 j))

    match map2iLeftValues m1 m2 f with
    | Error e -> failwithf "unexpected error %A" e
    | Ok result ->
        Assert.Equal(Ok(Some 23), get result 1UL<rowindex> 1UL<colindex>)
        Assert.Equal(1UL, uint64 result.nvals)
