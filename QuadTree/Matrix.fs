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
    | InvalidElementIndex


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

[<Struct>]
type CoordinateList<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val list: COOEntry<'value> list

    new(_nrows, _ncols, _list) =
        { nrows = _nrows
          ncols = _ncols
          list = _list }

let fromCoordinateList (coo: CoordinateList<'a>) =
    let nvals = (uint64 <| List.length coo.list) * 1UL<nvals>
    let nrows = coo.nrows
    let ncols = coo.ncols

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

    let tree = traverse coo.list (0UL<rowindex>, 0UL<colindex>) storageSize

    SparseMatrix(nrows, ncols, nvals, Storage(storageSize * 1UL<storageSize>, tree))

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

    CoordinateList(nrows, ncols, coo)

let empty nrows ncols =
    fromCoordinateList (CoordinateList(nrows, ncols, []))

let get (matrix: SparseMatrix<'a>) (row: uint64<rowindex>) (col: uint64<colindex>) : Result<option<'a>, Error> =
    if uint64 row >= uint64 matrix.nrows || uint64 col >= uint64 matrix.ncols then
        Error Error.InvalidElementIndex
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
    if uint64 row >= uint64 matrix.nrows || uint64 col >= uint64 matrix.ncols then
        Error Error.InvalidElementIndex
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

let map (matrix: SparseMatrix<_>) f =
    let rec inner (size: uint64<storageSize>) matrix =
        match matrix with
        | Leaf(Dummy) -> Leaf(Dummy), 0UL<nvals>
        | Leaf(UserValue(v)) ->
            let res = f v

            let nnz =
                match res with
                | None -> 0UL<nvals>
                | _ -> (uint64 size) * (uint64 size) * 1UL<nvals>

            Leaf(UserValue(res)), nnz
        | Node(x1, x2, x3, x4) ->
            let new_size = size / 2UL

            let t1, nvals1 = inner new_size x1
            let t2, nvals2 = inner new_size x2
            let t3, nvals3 = inner new_size x3
            let t4, nvals4 = inner new_size x4

            mkNode t1 t2 t3 t4, nvals1 + nvals2 + nvals3 + nvals4

    let storage, nvals = inner matrix.storage.size matrix.storage.data

    SparseMatrix(matrix.nrows, matrix.ncols, nvals, Storage(matrix.storage.size, storage))

let map2 (matrix1: SparseMatrix<_>) (matrix2: SparseMatrix<_>) f =
    let rec inner (size: uint64<storageSize>) matrix1 matrix2 =
        let _do x1 x2 x3 x4 y1 y2 y3 y4 =
            let new_size = size / 2UL

            match (inner new_size x1 y1), (inner new_size x2 y2), (inner new_size x3 y3), (inner new_size x4 y4) with
            | Ok((new_t1, nvals1)), Ok((new_t2, nvals2)), Ok((new_t3, nvals3)), Ok((new_t4, nvals4)) ->
                ((mkNode new_t1 new_t2 new_t3 new_t4), nvals1 + nvals2 + nvals3 + nvals4) |> Ok
            | Error(e), _, _, _
            | _, Error(e), _, _
            | _, _, Error(e), _
            | _, _, _, Error(e) -> Error(e)

        match (matrix1, matrix2) with
        | Leaf(Dummy), Leaf(Dummy) -> Ok(Leaf(Dummy), 0UL<nvals>)
        | Leaf(UserValue(v1)), Leaf(UserValue(v2)) ->
            let res = f v1 v2

            let nnz =
                match res with
                | None -> 0UL<nvals>
                | _ -> (uint64 size) * (uint64 size) * 1UL<nvals>

            (Leaf(UserValue(res)), nnz) |> Ok

        | Node(x1, x2, x3, x4), Node(y1, y2, y3, y4) -> _do x1 x2 x3 x4 y1 y2 y3 y4
        | Node(x1, x2, x3, x4), Leaf(v) -> _do x1 x2 x3 x4 matrix2 matrix2 matrix2 matrix2
        | Leaf(v), Node(x1, x2, x3, x4) -> _do matrix1 matrix1 matrix1 matrix1 x1 x2 x3 x4
        | (x, y) -> Error Error.InconsistentStructureOfStorages

    if matrix1.nrows = matrix2.nrows && matrix1.ncols = matrix2.ncols then
        match inner matrix1.storage.size matrix1.storage.data matrix2.storage.data with
        | Error x -> Error x
        | Ok(storage, nvals) ->
            (SparseMatrix(matrix1.nrows, matrix1.ncols, nvals, (Storage(matrix1.storage.size, storage))))
            |> Ok
    else
        Error Error.InconsistentSizeOfArguments

let map2i (matrix1: SparseMatrix<_>) (matrix2: SparseMatrix<_>) f =
    let rec inner (prow: uint64<rowindex>) (pcol: uint64<colindex>) (size: uint64<storageSize>) matrix1 matrix2 =
        match (matrix1, matrix2) with
        | Node(x1, x2, x3, x4), Node(y1, y2, y3, y4) ->
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            let t1, nvals1 = inner nwR nwC halfSize x1 y1
            let t2, nvals2 = inner neR neC halfSize x2 y2
            let t3, nvals3 = inner swR swC halfSize x3 y3
            let t4, nvals4 = inner seR seC halfSize x4 y4
            (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4
        | Node(x1, x2, x3, x4), Leaf(v2) ->
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            let t1, nvals1 = inner nwR nwC halfSize x1 (Leaf(v2))
            let t2, nvals2 = inner neR neC halfSize x2 (Leaf(v2))
            let t3, nvals3 = inner swR swC halfSize x3 (Leaf(v2))
            let t4, nvals4 = inner seR seC halfSize x4 (Leaf(v2))
            (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4
        | Leaf(v1), Node(y1, y2, y3, y4) ->
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            let t1, nvals1 = inner nwR nwC halfSize (Leaf(v1)) y1
            let t2, nvals2 = inner neR neC halfSize (Leaf(v1)) y2
            let t3, nvals3 = inner swR swC halfSize (Leaf(v1)) y3
            let t4, nvals4 = inner seR seC halfSize (Leaf(v1)) y4
            (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4
        | Leaf(Dummy), Leaf(Dummy) -> Leaf(Dummy), 0UL<nvals>
        | Leaf(UserValue(v1)), Leaf(UserValue(v2)) ->
            let res = f prow pcol v1 v2

            let nnz =
                match res with
                | Some _ -> 1UL<nvals>
                | None -> 0UL<nvals>

            Leaf(UserValue(res)), nnz
        | Leaf(UserValue(v)), Leaf(Dummy) ->
            let res = f prow pcol v None

            let nnz =
                match res with
                | Some _ -> 1UL<nvals>
                | None -> 0UL<nvals>

            Leaf(UserValue(res)), nnz
        | Leaf(Dummy), Leaf(UserValue(v)) ->
            let res = f prow pcol None v

            let nnz =
                match res with
                | Some _ -> 1UL<nvals>
                | None -> 0UL<nvals>

            Leaf(UserValue(res)), nnz

    if matrix1.nrows = matrix2.nrows && matrix1.ncols = matrix2.ncols then
        let storage, nvals =
            inner 0UL<rowindex> 0UL<colindex> matrix1.storage.size matrix1.storage.data matrix2.storage.data

        SparseMatrix(matrix1.nrows, matrix1.ncols, nvals, (Storage(matrix1.storage.size, storage)))
        |> Ok
    else
        Error Error.InconsistentSizeOfArguments

let mapi (matrix: SparseMatrix<'a>) f =
    let rec inner (prow: uint64<rowindex>) (pcol: uint64<colindex>) (size: uint64<storageSize>) matrix =
        match matrix with
        | Node(x1, x2, x3, x4) ->
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            let t1, nvals1 = inner nwR nwC halfSize x1
            let t2, nvals2 = inner neR neC halfSize x2
            let t3, nvals3 = inner swR swC halfSize x3
            let t4, nvals4 = inner seR seC halfSize x4
            (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4
        | Leaf(Dummy) -> Leaf(Dummy), 0UL<nvals>
        | Leaf(UserValue(v)) ->
            if size = 1UL<storageSize> then
                let res = f prow pcol v

                let nnz =
                    match res with
                    | Some _ -> 1UL<nvals>
                    | None -> 0UL<nvals>

                Leaf(UserValue(res)), nnz
            else
                let halfSize = size / 2UL

                let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                    getQuadrantCoords (prow, pcol) (uint64 halfSize)

                let t1, nvals1 = inner nwR nwC halfSize (Leaf(UserValue(v)))
                let t2, nvals2 = inner neR neC halfSize (Leaf(UserValue(v)))
                let t3, nvals3 = inner swR swC halfSize (Leaf(UserValue(v)))
                let t4, nvals4 = inner seR seC halfSize (Leaf(UserValue(v)))
                (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4

    let storage, nvals =
        inner 0UL<rowindex> 0UL<colindex> matrix.storage.size matrix.storage.data

    SparseMatrix(matrix.nrows, matrix.ncols, nvals, (Storage(matrix.storage.size, storage)))

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


let filter (matrix: SparseMatrix<'a>) (predicate: 'a -> bool) : SparseMatrix<'a> =
    let rec inner (prow: uint64<rowindex>) (pcol: uint64<colindex>) (size: uint64<storageSize>) matrix =
        match matrix with
        | Node(x1, x2, x3, x4) ->
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            let t1, nvals1 = inner nwR nwC halfSize x1
            let t2, nvals2 = inner neR neC halfSize x2
            let t3, nvals3 = inner swR swC halfSize x3
            let t4, nvals4 = inner seR seC halfSize x4
            (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4
        | Leaf(Dummy) -> Leaf(Dummy), 0UL<nvals>
        | Leaf(UserValue(None)) -> Leaf(UserValue(None)), 0UL<nvals>
        | Leaf(UserValue(Some(v))) ->
            if predicate v then
                Leaf(UserValue(Some v)), (uint64 size) * (uint64 size) * 1UL<nvals>
            else
                Leaf(UserValue(None)), 0UL<nvals>

    let storage, nvals =
        inner 0UL<rowindex> 0UL<colindex> matrix.storage.size matrix.storage.data

    SparseMatrix(matrix.nrows, matrix.ncols, nvals, (Storage(matrix.storage.size, storage)))

let exists (matrix: SparseMatrix<'a>) (predicate: 'a -> bool) : bool =
    let rec inner tree =
        match tree with
        | Leaf(Dummy) -> false
        | Leaf(UserValue(None)) -> false
        | Leaf(UserValue(Some(v))) -> predicate v
        | Node(nw, ne, sw, se) -> inner nw || inner ne || inner sw || inner se

    inner matrix.storage.data

let forall (matrix: SparseMatrix<'a>) (predicate: 'a -> bool) : bool =
    let rec inner tree =
        match tree with
        | Leaf(Dummy) -> true
        | Leaf(UserValue(None)) -> true
        | Leaf(UserValue(Some(v))) -> predicate v
        | Node(nw, ne, sw, se) -> inner nw && inner ne && inner sw && inner se

    inner matrix.storage.data
