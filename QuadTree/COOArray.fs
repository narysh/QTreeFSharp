module COOArray

open Common
open Matrix

let private compareEntriesByRowCol (e1: COOEntry<'v>) (e2: COOEntry<'v>) =
    let (i1, j1, _) = e1
    let (i2, j2, _) = e2
    let c = compare i1 i2
    if c <> 0 then c else compare j1 j2

let private entryComparer<'v> =
    { new System.Collections.Generic.IComparer<COOEntry<'v>> with
        member _.Compare(e1, e2) = compareEntriesByRowCol e1 e2 }

let private iterCells
    (nrows: uint64<nrows>)
    (ncols: uint64<ncols>)
    (action: uint64<rowindex> -> uint64<colindex> -> unit)
    =
    let mutable i = 0UL

    while i < uint64 nrows do
        let ri = i * 1UL<rowindex>
        let mutable j = 0UL

        while j < uint64 ncols do
            let cj = j * 1UL<colindex>
            action ri cj
            j <- j + 1UL

        i <- i + 1UL

[<Struct>]
type ArrayCOO<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val list: COOEntry<'value>[]

    static member private SortEntries(list: COOEntry<'value> seq) =
        list
        |> Seq.toArray
        |> Array.sortWith (fun (i1, j1, _) (i2, j2, _) ->
            let c = compare i1 i2
            if c <> 0 then c else compare j1 j2)

    new(_nrows, _ncols, _list: COOEntry<'value> seq) =
        { nrows = _nrows
          ncols = _ncols
          list = ArrayCOO<'value>.SortEntries(_list) }

    new(_nrows, _ncols, _list: COOEntry<'value>[], _presorted: bool) =
        let sorted =
            if _presorted then
                _list
            else
                ArrayCOO<'value>.SortEntries(_list)

        { nrows = _nrows
          ncols = _ncols
          list = sorted }

    static member Create(nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value>[]) : ArrayCOO<'value> =
        ArrayCOO<'value>(nrows, ncols, entries, true)

    member this.Item
        with get (rowindex: uint64<rowindex>, colindex: uint64<colindex>): Result<option<'value>, Error> =
            match checkBounds (uint64 this.nrows) (uint64 this.ncols) (uint64 rowindex) (uint64 colindex) with
            | Error e -> Error e
            | Ok() ->
                let idx =
                    System.Array.BinarySearch(
                        this.list,
                        (rowindex, colindex, Unchecked.defaultof<'value>),
                        entryComparer<'value>
                    )

                if idx >= 0 then
                    let (_, _, value) = this.list.[idx]
                    Ok(Some value)
                else
                    Ok None

let cooGet (coo: ArrayCOO<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>) : Result<option<'a>, Error> =
    coo.[rowindex, colindex]

let cooUpdate
    (coo: ArrayCOO<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>, value: 'a)
    : Result<ArrayCOO<'a>, Error> =
    match checkBounds (uint64 coo.nrows) (uint64 coo.ncols) (uint64 rowindex) (uint64 colindex) with
    | Error e -> Error e
    | Ok() ->
        let idx =
            System.Array.BinarySearch(coo.list, (rowindex, colindex, value), entryComparer<'a>)

        if idx >= 0 then
            let arr = Array.copy coo.list
            arr.[idx] <- (rowindex, colindex, value)
            Ok(ArrayCOO.Create(coo.nrows, coo.ncols, arr))
        else
            let insertAt = ~~~idx
            let arr = Array.zeroCreate (coo.list.Length + 1)
            Array.blit coo.list 0 arr 0 insertAt
            arr.[insertAt] <- (rowindex, colindex, value)
            Array.blit coo.list insertAt arr (insertAt + 1) (coo.list.Length - insertAt)

            Ok(ArrayCOO.Create(coo.nrows, coo.ncols, arr))

let private cooMapInner (coo: ArrayCOO<'a>) (op: UnaryOp<'a, 'b>) : ArrayCOO<'b> =
    match op with
    | UnaryOp.ValuesOnly f ->
        let res =
            coo.list
            |> Array.choose (fun (i, j, v) -> f v |> Option.map (fun r -> (i, j, r)))

        ArrayCOO.Create(coo.nrows, coo.ncols, res)
    | UnaryOp.ValuesOnlyIndexed f ->
        let res =
            coo.list
            |> Array.choose (fun (i, j, v) -> f i j v |> Option.map (fun r -> (i, j, r)))

        ArrayCOO.Create(coo.nrows, coo.ncols, res)
    | UnaryOp.AllCells f ->
        match f None with
        | None ->
            let res =
                coo.list
                |> Array.choose (fun (i, j, v) -> f (Some v) |> Option.map (fun r -> (i, j, r)))

            ArrayCOO.Create(coo.nrows, coo.ncols, res)
        | Some fnone ->
            let buf = ResizeArray<COOEntry<'b>>()
            let mutable ptr = 0

            iterCells coo.nrows coo.ncols (fun ri cj ->
                let v =
                    if ptr < coo.list.Length then
                        let (ei, ej, ev) = coo.list.[ptr]

                        if ei = ri && ej = cj then
                            ptr <- ptr + 1
                            Some ev
                        else
                            None
                    else
                        None

                match v with
                | Some value -> f (Some value)
                | None -> Some fnone
                |> Option.iter (fun value -> buf.Add((ri, cj, value))))

            ArrayCOO.Create(coo.nrows, coo.ncols, buf.ToArray())
    | UnaryOp.AllCellsIndexed f ->
        let buf = ResizeArray<COOEntry<'b>>()
        let mutable ptr = 0

        iterCells coo.nrows coo.ncols (fun ri cj ->
            let v =
                if ptr < coo.list.Length then
                    let (ei, ej, ev) = coo.list.[ptr]

                    if ei = ri && ej = cj then
                        ptr <- ptr + 1
                        Some ev
                    else
                        None
                else
                    None

            f ri cj v |> Option.iter (fun value -> buf.Add((ri, cj, value))))

        ArrayCOO.Create(coo.nrows, coo.ncols, buf.ToArray())

let private mergeBinary (a1: COOEntry<'a>[]) (a2: COOEntry<'b>[]) (op: BinaryOp<'a, 'b, 'c>) : COOEntry<'c>[] =
    let target = Array.zeroCreate (a1.Length + a2.Length)

    let rec loop p1 p2 outIdx =
        if p1 >= a1.Length && p2 >= a2.Length then
            outIdx
        elif p1 >= a1.Length then
            let (i, j, v2) = a2.[p2]

            match applyBinary op i j None (Some v2) with
            | Some r ->
                target.[outIdx] <- (i, j, r)
                loop p1 (p2 + 1) (outIdx + 1)
            | None -> loop p1 (p2 + 1) outIdx
        elif p2 >= a2.Length then
            let (i, j, v1) = a1.[p1]

            match applyBinary op i j (Some v1) None with
            | Some r ->
                target.[outIdx] <- (i, j, r)
                loop (p1 + 1) p2 (outIdx + 1)
            | None -> loop (p1 + 1) p2 outIdx
        else
            let (i1, j1, v1) = a1.[p1]
            let (i2, j2, v2) = a2.[p2]

            if i1 = i2 && j1 = j2 then
                match applyBinary op i1 j1 (Some v1) (Some v2) with
                | Some r ->
                    target.[outIdx] <- (i1, j1, r)
                    loop (p1 + 1) (p2 + 1) (outIdx + 1)
                | None -> loop (p1 + 1) (p2 + 1) outIdx
            elif (i1, j1) < (i2, j2) then
                match applyBinary op i1 j1 (Some v1) None with
                | Some r ->
                    target.[outIdx] <- (i1, j1, r)
                    loop (p1 + 1) p2 (outIdx + 1)
                | None -> loop (p1 + 1) p2 outIdx
            else
                match applyBinary op i2 j2 None (Some v2) with
                | Some r ->
                    target.[outIdx] <- (i2, j2, r)
                    loop p1 (p2 + 1) (outIdx + 1)
                | None -> loop p1 (p2 + 1) outIdx

    let total = loop 0 0 0

    if total = target.Length then
        target
    else
        Array.sub target 0 total

let private cooMap2Inner
    (coo1: ArrayCOO<'a>)
    (coo2: ArrayCOO<'b>)
    (op: BinaryOp<'a, 'b, 'c>)
    : Result<ArrayCOO<'c>, Error> =
    if uint64 coo1.nrows <> uint64 coo2.nrows || uint64 coo1.ncols <> uint64 coo2.ncols then
        Error Error.InconsistentSizeOfArguments
    else
        let nrows = coo1.nrows
        let ncols = coo1.ncols

        let result =
            match op with
            | BinaryOp.AllCells f ->
                match f None None with
                | None -> mergeBinary coo1.list coo2.list op
                | Some _ ->
                    let buf = ResizeArray<COOEntry<'c>>()
                    let mutable p1 = 0
                    let mutable p2 = 0

                    iterCells nrows ncols (fun ri cj ->
                        let v1 =
                            if p1 < coo1.list.Length then
                                let (ei, ej, ev) = coo1.list.[p1]

                                if ei = ri && ej = cj then
                                    p1 <- p1 + 1
                                    Some ev
                                else
                                    None
                            else
                                None

                        let v2 =
                            if p2 < coo2.list.Length then
                                let (ei, ej, ev) = coo2.list.[p2]

                                if ei = ri && ej = cj then
                                    p2 <- p2 + 1
                                    Some ev
                                else
                                    None
                            else
                                None

                        match f v1 v2 with
                        | Some value -> buf.Add((ri, cj, value))
                        | None -> ())

                    buf.ToArray()
            | BinaryOp.AllCellsIndexed f ->
                let buf = ResizeArray<COOEntry<'c>>()
                let mutable p1 = 0
                let mutable p2 = 0

                iterCells nrows ncols (fun ri cj ->
                    let v1 =
                        if p1 < coo1.list.Length then
                            let (ei, ej, ev) = coo1.list.[p1]

                            if ei = ri && ej = cj then
                                p1 <- p1 + 1
                                Some ev
                            else
                                None
                        else
                            None

                    let v2 =
                        if p2 < coo2.list.Length then
                            let (ei, ej, ev) = coo2.list.[p2]

                            if ei = ri && ej = cj then
                                p2 <- p2 + 1
                                Some ev
                            else
                                None
                        else
                            None

                    f ri cj v1 v2 |> Option.iter (fun value -> buf.Add((ri, cj, value))))

                buf.ToArray()
            | _ -> mergeBinary coo1.list coo2.list op

        ArrayCOO.Create(nrows, ncols, result) |> Ok

let cooMap (coo: ArrayCOO<'a>) f = cooMapInner coo (UnaryOp.AllCells f)

let cooMapValues (coo: ArrayCOO<'a>) f = cooMapInner coo (UnaryOp.ValuesOnly f)

let cooMapi (coo: ArrayCOO<'a>) f =
    cooMapInner coo (UnaryOp.AllCellsIndexed f)

let cooMapiValues (coo: ArrayCOO<'a>) f =
    cooMapInner coo (UnaryOp.ValuesOnlyIndexed f)

let cooMap2 (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCells f)

let cooMap2Values (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.ValuesOnly f)

let cooMap2AllCells (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCells f)

let cooMap2AtLeastOne (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AtLeastOneValue f)

let cooMap2LeftValues (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.LeftValuesOnly f)

let cooMap2i (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCellsIndexed f)

let cooMap2iValues (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.ValuesOnlyIndexed f)

let cooMap2iAllCells (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCellsIndexed f)

let cooMap2iAtLeastOne (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AtLeastOneValueIndexed f)

let cooMap2iLeftValues (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.LeftValuesOnlyIndexed f)

let mxmcoo
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (m1: ArrayCOO<'a>)
    (m2: ArrayCOO<'b>)
    =
    if uint64 m1.ncols <> uint64 m2.nrows then
        Error Error.InconsistentSizeOfArguments
    else
        let valuesOf (arr: COOEntry<'v>[]) =
            arr |> Array.map (fun (_, _, v) -> v) |> Array.distinct

        let canOptimize =
            let noneNone = op_mult None None = None

            let multSomeNone =
                valuesOf m1.list |> Array.forall (fun v -> op_mult (Some v) None = None)

            let multNoneSome =
                valuesOf m2.list |> Array.forall (fun v -> op_mult None (Some v) = None)

            noneNone && multSomeNone && multNoneSome

        let generalResult () =
            let m1Map =
                let mutable m = Map.empty

                for (i, j, v) in m1.list do
                    m <- Map.add (i, j) v m

                m

            let m2Map =
                let mutable m = Map.empty

                for (i, j, v) in m2.list do
                    m <- Map.add (i, j) v m

                m

            let kCount = uint64 m1.ncols
            let result = ResizeArray<COOEntry<'c>>()

            iterCells m1.nrows m2.ncols (fun ri cj ->
                let mutable acc = None
                let mutable k = 0UL

                while k < kCount do
                    let key1 = (ri, k * 1UL<colindex>)
                    let key2 = (k * 1UL<rowindex>, cj)
                    acc <- op_add acc (op_mult (Map.tryFind key1 m1Map) (Map.tryFind key2 m2Map))
                    k <- k + 1UL

                match acc with
                | Some value -> result.Add((ri, cj, value))
                | None -> ())

            ArrayCOO.Create(m1.nrows, m2.ncols, result.ToArray())

        if canOptimize then
            let rowStarts2 = ResizeArray<uint64<rowindex>>()
            let rowBegins2 = ResizeArray<int>()
            let rowEnds2 = ResizeArray<int>()
            let mutable idx = 0

            while idx < m2.list.Length do
                let (r, _, _) = m2.list.[idx]
                let beginIdx = idx
                let mutable advance = true

                while idx < m2.list.Length && advance do
                    let (r', _, _) = m2.list.[idx]

                    if r' = r then idx <- idx + 1 else advance <- false

                rowStarts2.Add(r)
                rowBegins2.Add(beginIdx)
                rowEnds2.Add(idx)

            let findRowSegments (k: uint64<rowindex>) =
                let mutable lo = 0
                let mutable hi = rowStarts2.Count - 1
                let mutable foundMid = -1

                while lo <= hi && foundMid < 0 do
                    let mid = (lo + hi) / 2

                    if rowStarts2.[mid] = k then foundMid <- mid
                    elif rowStarts2.[mid] < k then lo <- mid + 1
                    else hi <- mid - 1

                if foundMid >= 0 then
                    Some(rowBegins2.[foundMid], rowEnds2.[foundMid])
                else
                    None

            let products = ResizeArray<COOEntry<'c>>()
            let mutable baseIdx = 0

            while baseIdx < m1.list.Length do
                let (row, _, _) = m1.list.[baseIdx]
                let mutable nextIdx = baseIdx + 1
                let mutable advance = true

                while nextIdx < m1.list.Length && advance do
                    let (row', _, _) = m1.list.[nextIdx]

                    if row' = row then
                        nextIdx <- nextIdx + 1
                    else
                        advance <- false

                for e in baseIdx .. nextIdx - 1 do
                    let (_, k, v1) = m1.list.[e]
                    let kAsRow = uint64 k * 1UL<rowindex>

                    match findRowSegments kAsRow with
                    | Some(beginIdx, endIdx) ->
                        for q in beginIdx .. endIdx - 1 do
                            let (_, j, v2) = m2.list.[q]

                            match op_mult (Some v1) (Some v2) with
                            | Some product -> products.Add((row, j, product))
                            | None -> ()
                    | None -> ()

                baseIdx <- nextIdx

            let canMerge =
                let productValues =
                    products |> Seq.map (fun (_, _, v) -> v) |> Seq.distinct |> Seq.toArray

                productValues
                |> Array.forall (fun v -> op_add (Some v) None = Some v && op_add None (Some v) = Some v)

            if canMerge then
                let sortedProducts =
                    products.ToArray()
                    |> Array.sortWith (fun (i1, j1, _) (i2, j2, _) ->
                        let c = compare i1 i2

                        if c <> 0 then c else compare j1 j2)

                let result = ResizeArray<COOEntry<'c>>()
                let mutable q = 0

                while q < sortedProducts.Length do
                    let (i, j, v) = sortedProducts.[q]
                    let mutable sum = Some v
                    let mutable qq = q + 1
                    let mutable advance = true

                    while qq < sortedProducts.Length && advance do
                        let (i', j', v') = sortedProducts.[qq]

                        if i' = i && j' = j then
                            sum <- op_add sum (Some v')
                            qq <- qq + 1
                        else
                            advance <- false

                    match sum with
                    | Some value -> result.Add((i, j, value))
                    | None -> ()

                    q <- qq

                ArrayCOO.Create(m1.nrows, m2.ncols, result.ToArray()) |> Ok
            else
                generalResult () |> Ok
        else
            generalResult () |> Ok
