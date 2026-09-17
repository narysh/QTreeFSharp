module LinearAlgebra.Tests

open System
open Xunit

open Matrix
open COO
open Vector
open Common

(*
2,2,2,2
*
N,1,1,N
3,2,2,3
N,N,1,2
N,N,3,N
=
6,6,14,10
*)

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


let op_add_i x y =
    match (x, y) with
    | Some(a), Some(b) -> Some(min a b)
    | Some(a), _
    | _, Some(a) -> Some(a)
    | _ -> None

let op_mult_i (i, x) (row, col, y) = Some(i, row, col)


let leaf_v v = qtree.Leaf << UserValue <| Some v
let leaf_n () = qtree.Leaf << UserValue <| None
let leaf_d () = qtree.Leaf Dummy

let vleaf_v v =
    Vector.btree.Leaf << UserValue <| Some v

let vleaf_n () = Vector.btree.Leaf << UserValue <| None
let vleaf_d () = Vector.btree.Leaf Dummy

[<Fact>]
let ``Simple vxm. All sizes are power of two.`` () =
    let m =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(leaf_n (), leaf_v 1, leaf_v 3, leaf_v 2),
                Matrix.qtree.Node(leaf_v 1, leaf_n (), leaf_v 2, leaf_v 3),
                leaf_n (),
                Matrix.qtree.Node(leaf_v 1, leaf_v 2, leaf_v 3, leaf_n ())
            )

        let store = Matrix.Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 9UL<nvals>, store)

    let v =
        let tree = vleaf_v 2

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 4UL<nvals>, store)

    let expected =
        let tree = Vector.btree.Node(vleaf_v 6, Vector.btree.Node(vleaf_v 14, vleaf_v 10))

        let store = Vector.Storage(4UL<storageSize>, tree)
        Ok(SparseVector(4UL<dataLength>, 4UL<nvals>, store))

    let actual = LinearAlgebra.vxm op_add op_mult v m

    Assert.Equal(expected, actual)

(*
2,2,2,D
*
N,1,1,N
3,2,2,3
N,N,1,2
D,D,D,D
=
6,6,8,10
*)
[<Fact>]
let ``Simple vxm. 3 * (3x4)`` () =
    let m =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(leaf_n (), leaf_v 1, leaf_v 3, leaf_v 2),
                Matrix.qtree.Node(leaf_v 1, leaf_n (), leaf_v 2, leaf_v 3),
                Matrix.qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
                Matrix.qtree.Node(leaf_v 1, leaf_v 2, leaf_d (), leaf_d ())
            )

        let store = Matrix.Storage(4UL<storageSize>, tree)
        SparseMatrix(3UL<nrows>, 4UL<ncols>, 8UL<nvals>, store)

    let v =
        let tree = Vector.btree.Node(vleaf_v 2, Vector.btree.Node(vleaf_v 2, vleaf_d ()))

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(3UL<dataLength>, 3UL<nvals>, store)

    let expected =
        let tree = Vector.btree.Node(vleaf_v 6, Vector.btree.Node(vleaf_v 8, vleaf_v 10))

        let store = Vector.Storage(4UL<storageSize>, tree)
        Ok(SparseVector(4UL<dataLength>, 4UL<nvals>, store))

    let actual = LinearAlgebra.vxm op_add op_mult v m

    Assert.Equal(expected, actual)


(*
2,2,2,2
*
N,1,1,D
3,2,2,D
N,N,1,D
N,N,3,D
=
6,6,14,D
*)
[<Fact>]
let ``Simple vxm. 4 * (4x3).`` () =
    let m =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(leaf_n (), leaf_v 1, leaf_v 3, leaf_v 2),
                Matrix.qtree.Node(leaf_v 1, leaf_d (), leaf_v 2, leaf_d ()),
                leaf_n (),
                Matrix.qtree.Node(leaf_v 1, leaf_d (), leaf_v 3, leaf_d ())
            )

        let store = Matrix.Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 3UL<ncols>, 7UL<nvals>, store)

    let v =
        let tree = vleaf_v 2

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 4UL<nvals>, store)


    let expected =
        let tree = Vector.btree.Node(vleaf_v 6, Vector.btree.Node(vleaf_v 14, vleaf_d ()))

        let store = Vector.Storage(4UL<storageSize>, tree)
        Ok(SparseVector(3UL<dataLength>, 3UL<nvals>, store))

    let actual = LinearAlgebra.vxm op_add op_mult v m

    Assert.Equal(expected, actual)


(*
2,2,2,D
*
N,1,1,N,N,D,D,D
3,2,2,3,1,D,D,D
N,N,1,2,N,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
=
6,6,8,10,2,D,D,D
*)
[<Fact>]
let ``Simple vxm. 3 * (3x5)`` () =
    let m =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Node(leaf_n (), leaf_v 1, leaf_v 3, leaf_v 2),
                    Matrix.qtree.Node(leaf_v 1, leaf_n (), leaf_v 2, leaf_v 3),
                    Matrix.qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
                    Matrix.qtree.Node(leaf_v 1, leaf_v 2, leaf_d (), leaf_d ())
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Node(leaf_n (), leaf_d (), leaf_v 1, leaf_d ()),
                    leaf_d (),
                    Matrix.qtree.Node(leaf_n (), leaf_d (), leaf_d (), leaf_d ()),
                    leaf_d ()
                ),
                leaf_d (),
                leaf_d ()
            )

        let store = Matrix.Storage(8UL<storageSize>, tree)
        SparseMatrix(3UL<nrows>, 5UL<ncols>, 9UL<nvals>, store)

    let v =
        let tree = Vector.btree.Node(vleaf_v 2, Vector.btree.Node(vleaf_v 2, vleaf_d ()))

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(3UL<dataLength>, 3UL<nvals>, store)

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(vleaf_v 6, Vector.btree.Node(vleaf_v 8, vleaf_v 10)),
                Vector.btree.Node(Vector.btree.Node(vleaf_v 2, vleaf_d ()), vleaf_d ())
            )

        let store = Vector.Storage(8UL<storageSize>, tree)
        Ok(SparseVector(5UL<dataLength>, 5UL<nvals>, store))

    let actual = LinearAlgebra.vxm op_add op_mult v m

    Assert.Equal(expected, actual)

(*
2,2,2,D
*
N,1,1,N,N,D,D,D
3,2,2,3,1,D,D,D
N,N,1,2,N,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
=
// 6,6,8,10,2,D,D,D
(1,1,0),(0,0,1),(0,0,2),(1,1,3),(1,1,4)
*)
[<Fact>]
let ``Simple vxmi_values. 3 * (3x5)`` () =
    let m =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Node(leaf_n (), leaf_v 1, leaf_v 3, leaf_v 2),
                    Matrix.qtree.Node(leaf_v 1, leaf_n (), leaf_v 2, leaf_v 3),
                    Matrix.qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
                    Matrix.qtree.Node(leaf_v 1, leaf_v 2, leaf_d (), leaf_d ())
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Node(leaf_n (), leaf_d (), leaf_v 1, leaf_d ()),
                    leaf_d (),
                    Matrix.qtree.Node(leaf_n (), leaf_d (), leaf_d (), leaf_d ()),
                    leaf_d ()
                ),
                leaf_d (),
                leaf_d ()
            )

        let store = Matrix.Storage(8UL<storageSize>, tree)
        SparseMatrix(3UL<nrows>, 5UL<ncols>, 9UL<nvals>, store)

    let v =
        let tree = Vector.btree.Node(vleaf_v 2, Vector.btree.Node(vleaf_v 2, vleaf_d ()))

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(3UL<dataLength>, 3UL<nvals>, store)

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(
                    Vector.btree.Node(
                        vleaf_v (1UL<Vector.index>, 1UL<Matrix.rowindex>, 0UL<Matrix.colindex>),
                        vleaf_v (0UL<Vector.index>, 0UL<Matrix.rowindex>, 1UL<Matrix.colindex>)
                    ),
                    Vector.btree.Node(
                        vleaf_v (0UL<Vector.index>, 0UL<Matrix.rowindex>, 2UL<Matrix.colindex>),
                        vleaf_v (1UL<Vector.index>, 1UL<Matrix.rowindex>, 3UL<Matrix.colindex>)
                    )
                ),
                Vector.btree.Node(
                    Vector.btree.Node(
                        vleaf_v (1UL<Vector.index>, 1UL<Matrix.rowindex>, 4UL<Matrix.colindex>),
                        vleaf_d ()
                    ),
                    vleaf_d ()
                )
            )

        let store = Vector.Storage(8UL<storageSize>, tree)
        Ok(SparseVector(5UL<dataLength>, 5UL<nvals>, store))

    let actual = LinearAlgebra.vxmi_values op_add_i op_mult_i v m

    Assert.Equal(expected, actual)


(*
2,2,2,2
*
N,1,1,D
3,2,2,D
N,N,1,D
N,N,3,D
=
6,6,14,D
*)
[<Fact>]
let ``Simple vxmi_values. 4 * (4x3).`` () =
    let m =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(leaf_n (), leaf_v 1, leaf_v 3, leaf_v 2),
                Matrix.qtree.Node(leaf_v 1, leaf_d (), leaf_v 2, leaf_d ()),
                leaf_n (),
                Matrix.qtree.Node(leaf_v 1, leaf_d (), leaf_v 3, leaf_d ())
            )

        let store = Matrix.Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 3UL<ncols>, 7UL<nvals>, store)

    let v =
        let tree = vleaf_v 2

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 4UL<nvals>, store)


    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(
                    vleaf_v (1UL<Vector.index>, 1UL<Matrix.rowindex>, 0UL<Matrix.colindex>),
                    vleaf_v (0UL<Vector.index>, 0UL<Matrix.rowindex>, 1UL<Matrix.colindex>)
                ),
                Vector.btree.Node(vleaf_v (0UL<Vector.index>, 0UL<Matrix.rowindex>, 2UL<Matrix.colindex>), vleaf_d ())
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        Ok(SparseVector(3UL<dataLength>, 3UL<nvals>, store))

    let actual = LinearAlgebra.vxmi_values op_add_i op_mult_i v m

    Assert.Equal(expected, actual)


[<Fact>]
let ``Simple mxm`` () =
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

    let tree_expected =
        qtree.Node(
            leaf_v 12,
            qtree.Node(leaf_v 12, leaf_d (), leaf_v 12, leaf_d ()),
            qtree.Node(leaf_v 12, leaf_v 12, leaf_d (), leaf_d ()),
            qtree.Node(leaf_v 12, leaf_d (), leaf_d (), leaf_d ())
        )

    let m1 =
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 9UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let m2 = m1

    let expected =
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 9UL<nvals>, Matrix.Storage(4UL<storageSize>, tree_expected))

    let actual =
        match LinearAlgebra.mxm op_add op_mult m1 m2 with
        | Ok m -> m
        | _ -> failwith "Unreachable"

    Assert.Equal(expected.storage.data, actual.storage.data)

[<Fact>]
let ``Sparse mxm`` () =
    let m1 =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 1
              1UL<rowindex>, 1UL<colindex>, 2
              2UL<rowindex>, 2UL<colindex>, 3 ]

        let clist = Matrix.CoordinateList(3UL<nrows>, 3UL<ncols>, d)
        Matrix.fromCoordinateList clist

    let m2 =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 3
              1UL<rowindex>, 1UL<colindex>, 2
              2UL<rowindex>, 2UL<colindex>, 1 ]

        let clist = Matrix.CoordinateList(3UL<nrows>, 3UL<ncols>, d)
        Matrix.fromCoordinateList clist

    let expected =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 3
              1UL<rowindex>, 1UL<colindex>, 4
              2UL<rowindex>, 2UL<colindex>, 3 ]

        let clist = Matrix.CoordinateList(3UL<nrows>, 3UL<ncols>, d)
        Matrix.fromCoordinateList clist

    let actual =
        match LinearAlgebra.mxm op_add op_mult m1 m2 with
        | Ok m -> m
        | Error e -> failwith (e.ToString())

    Assert.Equal(expected, actual)

[<Fact>]
let ``Shrinking mxm`` () =
    // 2 x 3
    // 1 0 2
    // 0 3 0
    let m1 =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 1
              0UL<rowindex>, 2UL<colindex>, 2
              1UL<rowindex>, 1UL<colindex>, 3 ]

        let clist = Matrix.CoordinateList(2UL<nrows>, 3UL<ncols>, d)
        Matrix.fromCoordinateList clist

    // 3 x 2
    // 0 4
    // 5 0
    // 6 0
    let m2 =
        let d =
            [ 0UL<rowindex>, 1UL<colindex>, 4
              1UL<rowindex>, 0UL<colindex>, 5
              2UL<rowindex>, 0UL<colindex>, 6 ]

        let clist = Matrix.CoordinateList(3UL<nrows>, 2UL<ncols>, d)
        Matrix.fromCoordinateList clist

    // 2 x 2
    // 12 4
    // 15 0
    let expected =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 12
              0UL<rowindex>, 1UL<colindex>, 4
              1UL<rowindex>, 0UL<colindex>, 15 ]

        let clist = Matrix.CoordinateList(2UL<nrows>, 2UL<ncols>, d)
        Matrix.fromCoordinateList clist

    let actual =
        match LinearAlgebra.mxm op_add op_mult m1 m2 with
        | Ok m -> m
        | Error e -> failwith (e.ToString())

    Assert.Equal(expected, actual)

[<Fact>]
let ``Expanding mxm`` () =
    // 3 x 2
    // 1 0
    // 0 2
    // 3 0
    let m1 =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 1
              1UL<rowindex>, 1UL<colindex>, 2
              2UL<rowindex>, 0UL<colindex>, 3 ]

        let clist = Matrix.CoordinateList(3UL<nrows>, 2UL<ncols>, d)
        Matrix.fromCoordinateList clist
    // 2 x 3
    // 4 5 6
    // 0 0 0
    let m2 =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 4
              0UL<rowindex>, 1UL<colindex>, 5
              0UL<rowindex>, 2UL<colindex>, 6 ]

        let clist = Matrix.CoordinateList(2UL<nrows>, 3UL<ncols>, d)
        Matrix.fromCoordinateList clist

    // 3 x 3
    // 4 5 6
    // 0 0 0
    // 12 15 18
    let expected =
        let d =
            [ 0UL<rowindex>, 0UL<colindex>, 4
              0UL<rowindex>, 1UL<colindex>, 5
              0UL<rowindex>, 2UL<colindex>, 6
              2UL<rowindex>, 0UL<colindex>, 12
              2UL<rowindex>, 1UL<colindex>, 15
              2UL<rowindex>, 2UL<colindex>, 18 ]

        let clist = Matrix.CoordinateList(3UL<nrows>, 3UL<ncols>, d)
        Matrix.fromCoordinateList clist

    let actual =
        match LinearAlgebra.mxm op_add op_mult m1 m2 with
        | Ok m -> m
        | Error e -> failwith (e.ToString())

    Assert.Equal(expected, actual)
