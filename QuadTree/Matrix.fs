module Matrix

open Common

(*
| x1 | x2 |
----------
| x3 | x4 |
*)
type qtree<'value> =
    | Node of qtree<'value> * qtree<'value> * qtree<'value> * qtree<'value>
    | Leaf of treeValue<'value>

[<Measure>]
type ncols

[<Measure>]
type nrows

[<Struct>]
type Storage<'value> =
    // Storage is always size-x-size square.
    val size: uint64<storageSize>
    val data: qtree<'value>

    new(_size, _data) = { size = _size; data = _data }

[<Struct>]
type SparseMatrix<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val nvals: uint64<nvals>
    val storage: Storage<Option<'value>>

    new(_nrows, _ncols, _nvals, _storage) =
        { nrows = _nrows
          ncols = _ncols
          nvals = _nvals
          storage = _storage }

type Error =
    | InconsistentStructureOfStorages
    | InconsistentSizeOfArguments


let mkNode x1 x2 x3 x4 =
    match (x1, x2, x3, x4) with
    | Leaf(v1), Leaf(v2), Leaf(v3), Leaf(v4) when v1 = v2 && v2 = v3 && v3 = v4 -> Leaf(v1)
    | _ -> Node(x1, x2, x3, x4)

[<Measure>]
type rowindex

[<Measure>]
type colindex

let getQuadrantCoords (pr, pc) halfSize =
    (pr, pc), // NORTH WEST
    (pr, pc + halfSize * 1UL<colindex>), // NORTH EAST
    (pr + halfSize * 1UL<rowindex>, pc), // SOUTH WEST
    (pr + halfSize * 1UL<rowindex>, pc + halfSize * 1UL<colindex>) // SOUTH EAST

type COOEntry<'value> = uint64<rowindex> * uint64<colindex> * 'value

let compareCOOEntries (e1: COOEntry<'v>) (e2: COOEntry<'v>) =
    let (i1, j1, _) = e1
    let (i2, j2, _) = e2
    let c = compare i1 i2
    if c <> 0 then c else compare j1 j2

let validateCOOIndex
    (nrows: uint64<nrows>)
    (ncols: uint64<ncols>)
    (rowParamName: string)
    (colParamName: string)
    (rowindex: uint64<rowindex>)
    (colindex: uint64<colindex>)
    =
    if uint64 rowindex >= uint64 nrows then
        raise (System.ArgumentOutOfRangeException(rowParamName, "Row index is outside the matrix bounds."))
    elif uint64 colindex >= uint64 ncols then
        raise (System.ArgumentOutOfRangeException(colParamName, "Column index is outside the matrix bounds."))

[<Struct>]
type CoordinateList<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val list: COOEntry<'value> list

    new(_nrows, _ncols, _list) =
        { nrows = _nrows
          ncols = _ncols
          list = _list }

[<Struct>]
type ArrayCOO<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val list: COOEntry<'value>[]

    new(_nrows, _ncols, _list: COOEntry<'value> seq) =
        let sorted = _list |> Seq.toArray |> Array.sortWith compareCOOEntries

        { nrows = _nrows
          ncols = _ncols
          list = sorted }

    new(_nrows, _ncols, _list: COOEntry<'value>[], _presorted: bool) =
        let sorted =
            if _presorted then
                _list
            else
                _list |> Array.sortWith compareCOOEntries

        { nrows = _nrows
          ncols = _ncols
          list = sorted }

    // Fast factory: does NOT re-sort, expects an already sorted array.
    // Used by COOArray operations whose results are built in (row, col) order and
    // by cooUpdate, which maintains the sorted invariant itself.
    static member Create(nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value>[]) : ArrayCOO<'value> =
        ArrayCOO<'value>(nrows, ncols, entries, true)

let fromCoordinateList (coo: CoordinateList<'a>) =
    let unique =
        coo.list
        |> List.groupBy (fun (i, j, _) -> (i, j))
        |> List.map (fun ((i, j), entries) ->
            let value = entries |> List.map (fun (_, _, v) -> v) |> List.last
            (i, j, value))

    if
        unique
        |> List.exists (fun (i, j, _) -> uint64 i >= uint64 coo.nrows || uint64 j >= uint64 coo.ncols)
    then
        Error "Coordinates out of range"
    else
        let nvals = (uint64 <| List.length unique) * 1UL<nvals>
        let nrows = coo.nrows
        let ncols = coo.ncols

        // the resulting matrix is always square
        let storageSize = getNearestUpperPowerOfTwo (max (uint64 nrows) (uint64 ncols))

        let isEntryInQuadrant (pr, pc) size (entry: COOEntry<'a>) =
            let (i, j, _) = entry

            i >= pr
            && j >= pc
            && i < pr + size * 1UL<rowindex>
            && j < pc + size * 1UL<colindex>

        let rec traverse coordinates (pr, pc) size =
            match coordinates with
            | [] when (uint64 pr) + size < uint64 nrows && (uint64 pc) + size < uint64 ncols -> Leaf <| UserValue None
            | [] when uint64 pr >= uint64 nrows || uint64 pc >= uint64 ncols -> Leaf Dummy
            | (i, j, value) :: _ when pr = i && pc = j && size = 1UL -> Leaf << UserValue <| Some value
            | _ ->
                let halfSize = size / 2UL
                let nwp, nep, swp, sep = getQuadrantCoords (pr, pc) halfSize
                let nwCoo = coordinates |> List.filter (isEntryInQuadrant nwp halfSize)
                let neCoo = coordinates |> List.filter (isEntryInQuadrant nep halfSize)
                let swCoo = coordinates |> List.filter (isEntryInQuadrant swp halfSize)
                let seCoo = coordinates |> List.filter (isEntryInQuadrant sep halfSize)

                mkNode
                    (traverse nwCoo nwp halfSize)
                    (traverse neCoo nep halfSize)
                    (traverse swCoo swp halfSize)
                    (traverse seCoo sep halfSize)

        let tree = traverse unique (0UL<rowindex>, 0UL<colindex>) storageSize

        Ok(SparseMatrix(nrows, ncols, nvals, Storage(storageSize * 1UL<storageSize>, tree)))

let toCoordinateList (matrix: SparseMatrix<'a>) =
    let nrows = matrix.nrows
    let ncols = matrix.ncols

    let rec traverse tree (pr, pc) size =
        match tree with
        | Leaf Dummy
        | Leaf(UserValue None) -> []
        | Leaf(UserValue(Some value)) ->
            [ for i in uint64 pr .. (uint64 pr) + size - 1UL do
                  for j in uint64 pc .. (uint64 pc) + size - 1UL -> (i * 1UL<rowindex>, j * 1UL<colindex>, value) ]
        | Node(nw, ne, sw, se) ->
            let halfSize = size / 2UL
            let nwp, nep, swp, sep = getQuadrantCoords (pr, pc) halfSize

            traverse nw nwp halfSize
            @ traverse ne nep halfSize
            @ traverse sw swp halfSize
            @ traverse se sep halfSize

    let coo =
        traverse matrix.storage.data (0UL<rowindex>, 0UL<colindex>) (uint64 matrix.storage.size)

    let sorted = List.sort coo
    CoordinateList(nrows, ncols, sorted)

let empty nrows ncols =
    match fromCoordinateList (CoordinateList(nrows, ncols, [])) with
    | Ok m -> m
    | Error _ ->
        let storageSize =
            getNearestUpperPowerOfTwo (max (uint64 nrows) (uint64 ncols)) * 1UL<storageSize>

        SparseMatrix(nrows, ncols, 0UL<nvals>, Storage(storageSize, Leaf Dummy))

let get (matrix: SparseMatrix<'a>) (row: uint64<rowindex>) (col: uint64<colindex>) : Result<option<'a>, Error> =
    if uint64 row >= uint64 matrix.nrows then
        raise (System.ArgumentOutOfRangeException("row", "Row index is outside the matrix bounds."))
    elif uint64 col >= uint64 matrix.ncols then
        raise (System.ArgumentOutOfRangeException("col", "Column index is outside the matrix bounds."))
    else
        let rec inner tree (pr: uint64<rowindex>) (pc: uint64<colindex>) (size: uint64) =
            match tree with
            | Leaf Dummy -> None
            | Leaf(UserValue v) -> v
            | Node(nw, ne, sw, se) ->
                let halfSize = size / 2UL
                let midR = pr + halfSize * 1UL<rowindex>
                let midC = pc + halfSize * 1UL<colindex>

                if uint64 row < uint64 midR then
                    if uint64 col < uint64 midC then
                        inner nw pr pc halfSize
                    else
                        inner ne pr midC halfSize
                else if uint64 col < uint64 midC then
                    inner sw midR pc halfSize
                else
                    inner se midR midC halfSize

        Ok(inner matrix.storage.data (0UL<rowindex>) (0UL<colindex>) (uint64 matrix.storage.size))

let set
    (matrix: SparseMatrix<'a>)
    (row: uint64<rowindex>)
    (col: uint64<colindex>)
    (value: 'a)
    : Result<SparseMatrix<'a>, Error> =
    if uint64 row >= uint64 matrix.nrows then
        raise (System.ArgumentOutOfRangeException("row", "Row index is outside the matrix bounds."))
    elif uint64 col >= uint64 matrix.ncols then
        raise (System.ArgumentOutOfRangeException("col", "Column index is outside the matrix bounds."))
    else
        let rec inner tree (pr: uint64<rowindex>) (pc: uint64<colindex>) (size: uint64) =
            let halfSize = size / 2UL

            if size = 1UL then
                match tree with
                | Leaf(UserValue oldVal) ->
                    let newVal = Some value

                    let delta =
                        match newVal, oldVal with
                        | Some _, None -> 1L
                        | None, Some _ -> -1L
                        | _ -> 0L

                    Leaf(UserValue newVal), delta
                | Leaf Dummy -> Leaf(UserValue(Some value)), 1L
                | _ -> failwith "Unreachable"
            else
                let midR = pr + halfSize * 1UL<rowindex>
                let midC = pc + halfSize * 1UL<colindex>

                let (nw, ne, sw, se) =
                    match tree with
                    | Node(nw, ne, sw, se) -> nw, ne, sw, se
                    | Leaf v -> Leaf v, Leaf v, Leaf v, Leaf v

                let newChild, delta =
                    if uint64 row < uint64 midR then
                        if uint64 col < uint64 midC then
                            inner nw pr pc halfSize
                        else
                            inner ne pr midC halfSize
                    else if uint64 col < uint64 midC then
                        inner sw midR pc halfSize
                    else
                        inner se midR midC halfSize

                if uint64 row < uint64 midR then
                    if uint64 col < uint64 midC then
                        mkNode newChild ne sw se, delta
                    else
                        mkNode nw newChild sw se, delta
                else if uint64 col < uint64 midC then
                    mkNode nw ne newChild se, delta
                else
                    mkNode nw ne sw newChild, delta

        let storage, deltaNNZ =
            inner matrix.storage.data (0UL<rowindex>) (0UL<colindex>) (uint64 matrix.storage.size)

        let nvals = uint64 (int64 matrix.nvals + deltaNNZ) * 1UL<nvals>
        Ok(SparseMatrix(matrix.nrows, matrix.ncols, nvals, Storage(matrix.storage.size, storage)))

type UnaryOp<'a, 'b> =
    | ValuesOnly of ('a -> Option<'b>)
    | ValuesOnlyIndexed of (uint64<rowindex> -> uint64<colindex> -> 'a -> Option<'b>)
    | AllCells of (Option<'a> -> Option<'b>)
    | AllCellsIndexed of (uint64<rowindex> -> uint64<colindex> -> Option<'a> -> Option<'b>)

let private mapInner (matrix: SparseMatrix<'a>) (op: UnaryOp<'a, 'b>) : SparseMatrix<'b> =
    let rec inner
        (prow: uint64<rowindex>)
        (pcol: uint64<colindex>)
        (size: uint64<storageSize>)
        (tree: qtree<Option<'a>>)
        : qtree<Option<'b>> * uint64<nvals> =
        match tree with
        | Node(nw, ne, sw, se) ->
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            let t1, nvals1 = inner nwR nwC halfSize nw
            let t2, nvals2 = inner neR neC halfSize ne
            let t3, nvals3 = inner swR swC halfSize sw
            let t4, nvals4 = inner seR seC halfSize se

            mkNode t1 t2 t3 t4, nvals1 + nvals2 + nvals3 + nvals4
        | Leaf(Dummy) -> Leaf(Dummy), 0UL<nvals>
        | Leaf(UserValue(v)) ->
            match op with
            | UnaryOp.ValuesOnly f ->
                match v with
                | None -> Leaf(UserValue(None)), 0UL<nvals>
                | Some v' ->
                    let res = f v'

                    let nvals =
                        if res.IsSome then
                            (uint64 size) * (uint64 size) * 1UL<nvals>
                        else
                            0UL<nvals>

                    Leaf(UserValue(res)), nvals
            | UnaryOp.ValuesOnlyIndexed f ->
                match v with
                | None -> Leaf(UserValue(None)), 0UL<nvals>
                | Some v' ->
                    if size = 1UL<storageSize> then
                        let res = f prow pcol v'
                        let nvals = if res.IsSome then 1UL<nvals> else 0UL<nvals>
                        Leaf(UserValue(res)), nvals
                    else
                        let halfSize = size / 2UL

                        let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                            getQuadrantCoords (prow, pcol) (uint64 halfSize)

                        let t1, nvals1 = inner nwR nwC halfSize (Leaf(UserValue(v)))
                        let t2, nvals2 = inner neR neC halfSize (Leaf(UserValue(v)))
                        let t3, nvals3 = inner swR swC halfSize (Leaf(UserValue(v)))
                        let t4, nvals4 = inner seR seC halfSize (Leaf(UserValue(v)))
                        mkNode t1 t2 t3 t4, nvals1 + nvals2 + nvals3 + nvals4
            | UnaryOp.AllCells f ->
                let res = f v

                let nvals =
                    if res.IsSome then
                        (uint64 size) * (uint64 size) * 1UL<nvals>
                    else
                        0UL<nvals>

                Leaf(UserValue(res)), nvals
            | UnaryOp.AllCellsIndexed f ->
                if size = 1UL<storageSize> then
                    let res = f prow pcol v
                    let nvals = if res.IsSome then 1UL<nvals> else 0UL<nvals>
                    Leaf(UserValue(res)), nvals
                else
                    let halfSize = size / 2UL

                    let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                        getQuadrantCoords (prow, pcol) (uint64 halfSize)

                    let t1, nvals1 = inner nwR nwC halfSize (Leaf(UserValue(v)))
                    let t2, nvals2 = inner neR neC halfSize (Leaf(UserValue(v)))
                    let t3, nvals3 = inner swR swC halfSize (Leaf(UserValue(v)))
                    let t4, nvals4 = inner seR seC halfSize (Leaf(UserValue(v)))
                    mkNode t1 t2 t3 t4, nvals1 + nvals2 + nvals3 + nvals4

    let storage, nvals =
        inner 0UL<rowindex> 0UL<colindex> matrix.storage.size matrix.storage.data

    SparseMatrix(matrix.nrows, matrix.ncols, nvals, Storage(matrix.storage.size, storage))

let map (matrix: SparseMatrix<_>) f = mapInner matrix (UnaryOp.AllCells f)

let mapValues (matrix: SparseMatrix<'a>) f = mapInner matrix (UnaryOp.ValuesOnly f)

type AtLeastOne<'a, 'b> =
    | Both of 'a * 'b
    | Left of 'a
    | Right of 'b

type BinaryOp<'a, 'b, 'c> =
    | ValuesOnly of ('a -> 'b -> Option<'c>)
    | ValuesOnlyIndexed of (uint64<rowindex> -> uint64<colindex> -> 'a -> 'b -> Option<'c>)
    | AllCells of (Option<'a> -> Option<'b> -> Option<'c>)
    | AllCellsIndexed of (uint64<rowindex> -> uint64<colindex> -> Option<'a> -> Option<'b> -> Option<'c>)
    | AtLeastOneValue of (AtLeastOne<'a, 'b> -> Option<'c>)
    | AtLeastOneValueIndexed of (uint64<rowindex> -> uint64<colindex> -> AtLeastOne<'a, 'b> -> Option<'c>)
    | LeftValuesOnly of ('a -> Option<'b> -> Option<'c>)
    | LeftValuesOnlyIndexed of (uint64<rowindex> -> uint64<colindex> -> 'a -> Option<'b> -> Option<'c>)

let applyBinary
    (op: BinaryOp<'a, 'b, 'c>)
    (prow: uint64<rowindex>)
    (pcol: uint64<colindex>)
    (v1: Option<'a>)
    (v2: Option<'b>)
    : Option<'c> =
    match op with
    | BinaryOp.ValuesOnly f ->
        match v1, v2 with
        | Some a, Some b -> f a b
        | _ -> None
    | BinaryOp.ValuesOnlyIndexed f ->
        match v1, v2 with
        | Some a, Some b -> f prow pcol a b
        | _ -> None
    | BinaryOp.AllCells f -> f v1 v2
    | BinaryOp.AllCellsIndexed f -> f prow pcol v1 v2
    | BinaryOp.AtLeastOneValue f ->
        match v1, v2 with
        | Some a, Some b -> f (AtLeastOne.Both(a, b))
        | Some a, None -> f (AtLeastOne.Left a)
        | None, Some b -> f (AtLeastOne.Right b)
        | None, None -> None
    | BinaryOp.AtLeastOneValueIndexed f ->
        match v1, v2 with
        | Some a, Some b -> f prow pcol (AtLeastOne.Both(a, b))
        | Some a, None -> f prow pcol (AtLeastOne.Left a)
        | None, Some b -> f prow pcol (AtLeastOne.Right b)
        | None, None -> None
    | BinaryOp.LeftValuesOnly f ->
        match v1 with
        | Some a -> f a v2
        | None -> None
    | BinaryOp.LeftValuesOnlyIndexed f ->
        match v1 with
        | Some a -> f prow pcol a v2
        | None -> None

let private isIndexedBinary (op: BinaryOp<'a, 'b, 'c>) =
    match op with
    | BinaryOp.ValuesOnlyIndexed _
    | BinaryOp.AllCellsIndexed _
    | BinaryOp.AtLeastOneValueIndexed _
    | BinaryOp.LeftValuesOnlyIndexed _ -> true
    | _ -> false

let private map2Inner
    (matrix1: SparseMatrix<'a>)
    (matrix2: SparseMatrix<'b>)
    (op: BinaryOp<'a, 'b, 'c>)
    : Result<SparseMatrix<'c>, Error> =
    let rec inner
        (prow: uint64<rowindex>)
        (pcol: uint64<colindex>)
        (size: uint64<storageSize>)
        (tree1: qtree<Option<'a>>)
        (tree2: qtree<Option<'b>>)
        : Result<qtree<Option<'c>> * uint64<nvals>, Error> =
        let split
            (x1: qtree<Option<'a>>)
            (x2: qtree<Option<'a>>)
            (x3: qtree<Option<'a>>)
            (x4: qtree<Option<'a>>)
            (y1: qtree<Option<'b>>)
            (y2: qtree<Option<'b>>)
            (y3: qtree<Option<'b>>)
            (y4: qtree<Option<'b>>)
            =
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            match
                (inner nwR nwC halfSize x1 y1),
                (inner neR neC halfSize x2 y2),
                (inner swR swC halfSize x3 y3),
                (inner seR seC halfSize x4 y4)
            with
            | Ok(t1, nvals1), Ok(t2, nvals2), Ok(t3, nvals3), Ok(t4, nvals4) ->
                Ok(mkNode t1 t2 t3 t4, nvals1 + nvals2 + nvals3 + nvals4)
            | Error e, _, _, _
            | _, Error e, _, _
            | _, _, Error e, _
            | _, _, _, Error e -> Error e

        match tree1, tree2 with
        | Node(x1, x2, x3, x4), Node(y1, y2, y3, y4) -> split x1 x2 x3 x4 y1 y2 y3 y4
        | Node(x1, x2, x3, x4), Leaf(v2) -> split x1 x2 x3 x4 (Leaf(v2)) (Leaf(v2)) (Leaf(v2)) (Leaf(v2))
        | Leaf(v1), Node(y1, y2, y3, y4) -> split (Leaf(v1)) (Leaf(v1)) (Leaf(v1)) (Leaf(v1)) y1 y2 y3 y4
        | Leaf(Dummy), Leaf(Dummy) -> Ok(Leaf(Dummy), 0UL<nvals>)
        | Leaf(UserValue(v1)), Leaf(UserValue(v2)) ->
            if size > 1UL<storageSize> && isIndexedBinary op then
                split
                    (Leaf(UserValue(v1)))
                    (Leaf(UserValue(v1)))
                    (Leaf(UserValue(v1)))
                    (Leaf(UserValue(v1)))
                    (Leaf(UserValue(v2)))
                    (Leaf(UserValue(v2)))
                    (Leaf(UserValue(v2)))
                    (Leaf(UserValue(v2)))
            else
                let res = applyBinary op prow pcol v1 v2

                let nnz =
                    if res.IsSome then
                        (uint64 size) * (uint64 size) * 1UL<nvals>
                    else
                        0UL<nvals>

                Ok(Leaf(UserValue(res)), nnz)
        | _ -> Error Error.InconsistentStructureOfStorages

    if matrix1.nrows = matrix2.nrows && matrix1.ncols = matrix2.ncols then
        inner 0UL<rowindex> 0UL<colindex> matrix1.storage.size matrix1.storage.data matrix2.storage.data
        |> Result.map (fun (storage, nvals) ->
            SparseMatrix(matrix1.nrows, matrix1.ncols, nvals, Storage(matrix1.storage.size, storage)))
    else
        Error Error.InconsistentSizeOfArguments

let map2 (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    map2Inner matrix1 matrix2 (BinaryOp.AllCells f)

let map2Values (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    map2Inner matrix1 matrix2 (BinaryOp.ValuesOnly f)

let map2AllCells (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    map2Inner matrix1 matrix2 (BinaryOp.AllCells f)

let map2AtLeastOne (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    map2Inner matrix1 matrix2 (BinaryOp.AtLeastOneValue f)

let map2LeftValues (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    map2Inner matrix1 matrix2 (BinaryOp.LeftValuesOnly f)

let map2i (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    map2Inner matrix1 matrix2 (BinaryOp.AllCellsIndexed f)

let map2iValues (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    map2Inner matrix1 matrix2 (BinaryOp.ValuesOnlyIndexed f)

let map2iAllCells (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    map2Inner matrix1 matrix2 (BinaryOp.AllCellsIndexed f)

let map2iAtLeastOne (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    map2Inner matrix1 matrix2 (BinaryOp.AtLeastOneValueIndexed f)

let map2iLeftValues (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    map2Inner matrix1 matrix2 (BinaryOp.LeftValuesOnlyIndexed f)

let mapi (matrix: SparseMatrix<'a>) f =
    mapInner matrix (UnaryOp.AllCellsIndexed f)

let mapiValues (matrix: SparseMatrix<'a>) f =
    mapInner matrix (UnaryOp.ValuesOnlyIndexed f)

let foldAssociative (folder: 'T option -> 'T option -> 'T option) (state: 'T option) (matrix: SparseMatrix<'T>) =
    let rec traverse tree (size: uint64<storageSize>) (state: 'T option) =
        match tree with
        | Leaf Dummy -> state
        | Leaf(UserValue v) ->
            let area = (uint64 size) * (uint64 size)

            let rec foldValue size accum =
                if size = 1UL then
                    accum
                else
                    let halfSize = size / 2UL

                    foldValue halfSize (folder accum accum)

            folder state (foldValue area v)
        | Node(nw, ne, sw, se) ->
            let halfSize = size / 2UL

            let nwState = traverse nw halfSize state
            let neState = traverse ne halfSize nwState
            let swState = traverse sw halfSize neState
            let seState = traverse se halfSize swState

            seState

    let storageSize = matrix.storage.size

    let tree = matrix.storage.data
    traverse tree storageSize state


let getLowerTriangle (matrix: SparseMatrix<_>) =

    // returns tree, removed_nvals
    let rec makeNone tree (size: uint64<storageSize>) =
        match tree with
        | Leaf Dummy
        | Leaf(UserValue None) -> tree, 0UL<nvals>
        | Leaf(UserValue(Some _)) -> Leaf(UserValue None), (uint64 <| size * size) * 1UL<nvals>
        | Node(nw, ne, sw, se) ->
            let halfSize = size / 2UL
            let nw_new, nw_removed = makeNone nw halfSize
            let ne_new, ne_removed = makeNone ne halfSize
            let sw_new, sw_removed = makeNone sw halfSize
            let se_new, se_removed = makeNone se halfSize

            (mkNode nw_new ne_new sw_new se_new), nw_removed + ne_removed + sw_removed + se_removed

    let rec traverse tree size =
        match tree with
        | Leaf _ when size = 1UL<storageSize> -> tree, 0UL<nvals>
        | Leaf Dummy -> Leaf Dummy, 0UL<nvals>
        | Leaf _ ->
            let halfSize = size / 2UL

            let nw, nw_removed = traverse tree halfSize

            let ne, ne_removed =
                Leaf <| UserValue None, (uint64 <| halfSize * halfSize) * 1UL<nvals>

            let sw, sw_removed = tree, 0UL<nvals>
            let se, se_removed = traverse tree halfSize
            (mkNode nw ne sw se), nw_removed + ne_removed + sw_removed + se_removed
        | Node(nw, ne, sw, se) ->
            let halfSize = size / 2UL

            let nw_new, nw_removed = traverse nw halfSize
            let ne_new, ne_removed = makeNone ne halfSize
            let sw_new, sw_removed = sw, 0UL<nvals>
            let se_new, se_removed = traverse se halfSize

            (mkNode nw_new ne_new sw_new se_new), nw_removed + ne_removed + sw_removed + se_removed

    let storageSize = matrix.storage.size
    let tree, nvals_removed = traverse matrix.storage.data storageSize

    SparseMatrix(matrix.nrows, matrix.ncols, matrix.nvals - nvals_removed, Storage(storageSize, tree))

let transpose (matrix: SparseMatrix<_>) =
    let rec traverse tree =
        match tree with
        | Leaf _ -> tree
        | Node(nw, ne, sw, se) ->
            mkNode
                (traverse nw)
                (traverse sw) // ne -> sw
                (traverse ne) // sw -> ne
                (traverse se)

    let nrows = (uint64 matrix.ncols) * 1UL<nrows>
    let ncols = (uint64 matrix.nrows) * 1UL<ncols>

    let tree = traverse matrix.storage.data

    SparseMatrix(nrows, ncols, matrix.nvals, Storage(matrix.storage.size, tree))

let mask (m1: SparseMatrix<'a>) (m2: SparseMatrix<'b>) f =
    map2 m1 m2 (fun m1 m2 -> if f m2 then m1 else None)


let slice
    (matrix: SparseMatrix<'a>)
    (rowStart: int)
    (rowEnd: int)
    (colStart: int)
    (colEnd: int)
    : Result<SparseMatrix<'a>, string> =
    if rowStart < 0 then
        Error "Start row should be >= 0"
    elif rowEnd < 0 then
        Error "End row should be >= 0"
    elif colStart < 0 then
        Error "Start column should be >= 0"
    elif colEnd < 0 then
        Error "End column should be >= 0"
    elif rowStart > int matrix.nrows - 1 then
        Error "Start row is out of matrix length"
    elif rowEnd > int matrix.nrows - 1 then
        Error "End row is out of matrix length"
    elif colStart > int matrix.ncols - 1 then
        Error "Start column is out of matrix length"
    elif colEnd > int matrix.ncols - 1 then
        Error "End column is out of matrix length"
    elif rowStart > rowEnd then
        Error "Start row should be <= end row"
    elif colStart > colEnd then
        Error "Start column should be <= end column"
    else
        let rowStartIdx = uint64 rowStart * 1UL<rowindex>
        let rowEndIdx = uint64 rowEnd * 1UL<rowindex>
        let colStartIdx = uint64 colStart * 1UL<colindex>
        let colEndIdx = uint64 colEnd * 1UL<colindex>
        let newRows = uint64 (rowEnd - rowStart + 1) * 1UL<nrows>
        let newCols = uint64 (colEnd - colStart + 1) * 1UL<ncols>

        let newSize =
            getNearestUpperPowerOfTwo (max (uint64 newRows) (uint64 newCols))
            * 1UL<storageSize>

        let rec narrowOld
            (oldRow: uint64<rowindex>)
            (oldCol: uint64<colindex>)
            (oldSize: uint64<storageSize>)
            (oldTree: qtree<Option<'a>>)
            (qRowStart: uint64<rowindex>)
            (qRowEnd: uint64<rowindex>)
            (qColStart: uint64<colindex>)
            (qColEnd: uint64<colindex>)
            : struct (uint64<rowindex> * uint64<colindex> * uint64<storageSize> * qtree<Option<'a>>) =
            match oldTree with
            | Leaf _ -> struct (oldRow, oldCol, oldSize, oldTree)
            | Node(nw, ne, sw, se) ->
                let half = oldSize / 2UL
                let halfRow = (uint64 half) * 1UL<rowindex>
                let halfCol = (uint64 half) * 1UL<colindex>
                let midRow = oldRow + halfRow
                let midCol = oldCol + halfCol

                let crossesRow = qRowStart < midRow && qRowEnd >= midRow
                let crossesCol = qColStart < midCol && qColEnd >= midCol

                if crossesRow || crossesCol then
                    struct (oldRow, oldCol, oldSize, oldTree)
                elif qRowEnd < midRow && qColEnd < midCol then
                    narrowOld oldRow oldCol half nw qRowStart qRowEnd qColStart qColEnd
                elif qRowEnd < midRow && qColStart >= midCol then
                    narrowOld oldRow midCol half ne qRowStart qRowEnd qColStart qColEnd
                elif qRowStart >= midRow && qColEnd < midCol then
                    narrowOld midRow oldCol half sw qRowStart qRowEnd qColStart qColEnd
                else
                    narrowOld midRow midCol half se qRowStart qRowEnd qColStart qColEnd

        let rec buildNewTree
            (targetSize: uint64<storageSize>)
            (relRow: uint64<rowindex>)
            (relCol: uint64<colindex>)
            (oldRow: uint64<rowindex>)
            (oldCol: uint64<colindex>)
            (oldSize: uint64<storageSize>)
            (oldTree: qtree<Option<'a>>)
            : struct (qtree<Option<'a>> * uint64<nvals>) =

            let absRow = rowStartIdx + relRow
            let absCol = colStartIdx + relCol
            let absRowEnd = absRow + (uint64 targetSize * 1UL<rowindex>) - 1UL<rowindex>
            let absColEnd = absCol + (uint64 targetSize * 1UL<colindex>) - 1UL<colindex>

            if absRow > rowEndIdx || absCol > colEndIdx then
                struct (Leaf Dummy, 0UL<nvals>)
            else
                let queryRowEnd = if absRowEnd > rowEndIdx then rowEndIdx else absRowEnd
                let queryColEnd = if absColEnd > colEndIdx then colEndIdx else absColEnd

                let struct (nRow, nCol, nSize, nTree) =
                    narrowOld oldRow oldCol oldSize oldTree absRow queryRowEnd absCol queryColEnd

                let fullyInBounds = absRowEnd <= rowEndIdx && absColEnd <= colEndIdx

                match nTree with
                | Leaf Dummy -> struct (Leaf Dummy, 0UL<nvals>)
                | Leaf(UserValue None) when fullyInBounds -> struct (Leaf(UserValue None), 0UL<nvals>)
                | Leaf(UserValue(Some v)) when fullyInBounds ->
                    struct (Leaf(UserValue(Some v)), (uint64 targetSize) * (uint64 targetSize) * 1UL<nvals>)
                | _ ->
                    if targetSize = 1UL<storageSize> then
                        struct (Leaf Dummy, 0UL<nvals>)
                    else
                        let half = targetSize / 2UL
                        let halfRow = (uint64 half) * 1UL<rowindex>
                        let halfCol = (uint64 half) * 1UL<colindex>

                        let struct (nwTree, nwNvals) = buildNewTree half relRow relCol nRow nCol nSize nTree

                        let struct (neTree, neNvals) =
                            buildNewTree half relRow (relCol + halfCol) nRow nCol nSize nTree

                        let struct (swTree, swNvals) =
                            buildNewTree half (relRow + halfRow) relCol nRow nCol nSize nTree

                        let struct (seTree, seNvals) =
                            buildNewTree half (relRow + halfRow) (relCol + halfCol) nRow nCol nSize nTree

                        let totalNvals = nwNvals + neNvals + swNvals + seNvals

                        if totalNvals = 0UL<nvals> then
                            struct (Leaf Dummy, 0UL<nvals>)
                        else
                            struct (mkNode nwTree neTree swTree seTree, totalNvals)

        let struct (finalTree, finalNvals) =
            buildNewTree
                newSize
                0UL<rowindex>
                0UL<colindex>
                0UL<rowindex>
                0UL<colindex>
                matrix.storage.size
                matrix.storage.data

        Ok(SparseMatrix(newRows, newCols, finalNvals, Storage(newSize, finalTree)))

let foldQuadtree folder state size tree =
    let rec inner rowOffset colOffset currentSize subTree acc =
        match subTree with
        | Leaf Dummy -> acc
        | Leaf(UserValue None) -> acc
        | Leaf(UserValue(Some value)) ->
            let rec loop currRow currCol currentAcc =
                if currRow = rowOffset + currentSize then
                    currentAcc
                elif currCol = colOffset + currentSize then
                    loop (currRow + 1UL) colOffset currentAcc
                else
                    loop currRow (currCol + 1UL) (folder currentAcc currRow currCol value)

            loop rowOffset colOffset acc
        | Node(nw, ne, sw, se) ->
            let half = currentSize / 2UL
            let acc1 = inner rowOffset colOffset half nw acc
            let acc2 = inner rowOffset (colOffset + half) half ne acc1
            let acc3 = inner (rowOffset + half) colOffset half sw acc2
            inner (rowOffset + half) (colOffset + half) half se acc3

    inner 0UL 0UL (uint64 size) tree state

let private reduceAlongAxis
    (bucketCount: int)
    (pickBucket: uint64 -> uint64 -> int)
    (op: 'a option -> 'a option -> 'a option)
    (matrix: SparseMatrix<'a>)
    : Vector.SparseVector<'a> =
    let length = uint64 bucketCount * 1UL<Vector.dataLength>
    let buckets = Array.init bucketCount (fun _ -> ResizeArray<'a>())

    foldQuadtree (fun _ row col v -> buckets.[pickBucket row col].Add(v)) () matrix.storage.size matrix.storage.data

    let vectorData =
        buckets
        |> Array.mapi (fun idx bucket ->
            if bucket.Count = 0 then
                None
            else
                let mutable acc = Some bucket.[0]

                for i in 1 .. bucket.Count - 1 do
                    acc <- op acc (Some bucket.[i])

                Some(uint64 idx * 1UL<Vector.index>, acc.Value))
        |> Array.choose id
        |> Array.toList

    match Vector.fromCoordinateList (Vector.CoordinateList(length, vectorData)) with
    | Ok v -> v
    | Error _ -> Vector.SparseVector(length, 0UL<nvals>, Vector.Storage(1UL<storageSize>, Vector.Leaf Dummy))

let reduceRows (op: 'a option -> 'a option -> 'a option) (matrix: SparseMatrix<'a>) : Vector.SparseVector<'a> =
    reduceAlongAxis (int matrix.nrows) (fun row _ -> int row) op matrix

let reduceCols (op: 'a option -> 'a option -> 'a option) (matrix: SparseMatrix<'a>) : Vector.SparseVector<'a> =
    reduceAlongAxis (int matrix.ncols) (fun _ col -> int col) op matrix

let kroneckerProduct
    (matrixA: SparseMatrix<'a>)
    (matrixB: SparseMatrix<'b>)
    (combine: 'a -> 'b -> 'c option)
    : Result<SparseMatrix<'c>, string> =

    let rowsB = uint64 matrixB.nrows
    let colsB = uint64 matrixB.ncols

    let resultRows = (uint64 matrixA.nrows * rowsB) * 1UL<nrows>
    let resultCols = (uint64 matrixA.ncols * colsB) * 1UL<ncols>

    let storageSize =
        getNearestUpperPowerOfTwo (max (uint64 resultRows) (uint64 resultCols))
        * 1UL<storageSize>

    let collectEntries (matrix: SparseMatrix<'T>) =
        let result = ResizeArray<struct (uint64 * uint64 * 'T)>()
        let maxRows = uint64 matrix.nrows
        let maxCols = uint64 matrix.ncols

        let rec traverse (row: uint64) (col: uint64) (size: uint64) (tree: qtree<Option<'T>>) =
            if row >= maxRows || col >= maxCols then
                ()
            else
                match tree with
                | Leaf Dummy -> ()
                | Leaf(UserValue None) -> ()
                | Leaf(UserValue(Some value)) ->
                    if size = 1UL then
                        result.Add(struct (row, col, value))
                    else
                        let endRow = min (row + size) maxRows
                        let endCol = min (col + size) maxCols

                        for r in row .. endRow - 1UL do
                            for c in col .. endCol - 1UL do
                                result.Add(struct (r, c, value))
                | Node(nw, ne, sw, se) ->
                    let half = size / 2UL
                    traverse row col half nw
                    traverse row (col + half) half ne
                    traverse (row + half) col half sw
                    traverse (row + half) (col + half) half se

        traverse 0UL 0UL (uint64 matrix.storage.size) matrix.storage.data
        result.ToArray()

    let entriesA = collectEntries matrixA
    let entriesB = collectEntries matrixB

    let maxNNZ = entriesA.Length * entriesB.Length
    let buffer = Array.zeroCreate<struct (uint64 * uint64 * 'c)> (maxNNZ)
    let mutable actualCount = 0

    for i in 0 .. entriesA.Length - 1 do
        let struct (rowA, colA, valueA) = entriesA.[i]
        let baseRow = rowA * rowsB
        let baseCol = colA * colsB

        for j in 0 .. entriesB.Length - 1 do
            let struct (rowB, colB, valueB) = entriesB.[j]

            match combine valueA valueB with
            | Some v ->
                buffer.[actualCount] <- struct (baseRow + rowB, baseCol + colB, v)
                actualCount <- actualCount + 1
            | None -> ()

    let partitionRow (array: struct (uint64 * uint64 * 'c)[]) (left: int) (right: int) (midRow: uint64) =
        let mutable i = left

        for j in left .. right - 1 do
            let struct (r, _, _) = array.[j]

            if r < midRow then
                let tmp = array.[i]
                array.[i] <- array.[j]
                array.[j] <- tmp
                i <- i + 1

        i

    let partitionCol (array: struct (uint64 * uint64 * 'c)[]) (left: int) (right: int) (midCol: uint64) =
        let mutable i = left

        for j in left .. right - 1 do
            let struct (_, c, _) = array.[j]

            if c < midCol then
                let tmp = array.[i]
                array.[i] <- array.[j]
                array.[j] <- tmp
                i <- i + 1

        i

    let rec buildTree
        (array: struct (uint64 * uint64 * 'c)[])
        (left: int)
        (right: int)
        (row: uint64)
        (col: uint64)
        (size: uint64)
        : qtree<Option<'c>> =
        if left >= right then
            Leaf Dummy
        elif size = 1UL then
            let struct (_, _, value) = array.[left]
            Leaf(UserValue(Some value))
        else
            let half = size / 2UL
            let midRow = row + half
            let midCol = col + half

            let topEnd = partitionRow array left right midRow
            let nwEnd = partitionCol array left topEnd midCol
            let swEnd = partitionCol array topEnd right midCol

            let nwTree = buildTree array left nwEnd row col half
            let neTree = buildTree array nwEnd topEnd row midCol half
            let swTree = buildTree array topEnd swEnd midRow col half
            let seTree = buildTree array swEnd right midRow midCol half

            mkNode nwTree neTree swTree seTree

    let finalTree = buildTree buffer 0 actualCount 0UL 0UL (uint64 storageSize)
    let finalNvals = uint64 actualCount * 1UL<nvals>
    Ok(SparseMatrix(resultRows, resultCols, finalNvals, Storage(storageSize, finalTree)))
