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
            let totalCells = int (uint64 coo.nrows * uint64 coo.ncols)
            let target = Array.zeroCreate totalCells

            let rec loopRow i ptr outIdx =
                if i < uint64 coo.nrows then
                    let ri = i * 1UL<rowindex>

                    let rec loopCol j curPtr curOut =
                        if j < uint64 coo.ncols then
                            let cj = j * 1UL<colindex>

                            let v, nextPtr =
                                if curPtr < coo.list.Length then
                                    let (ei, ej, ev) = coo.list.[curPtr]

                                    if ei = ri && ej = cj then
                                        Some ev, curPtr + 1
                                    else
                                        None, curPtr
                                else
                                    None, curPtr

                            let resVal =
                                match v with
                                | Some value -> f (Some value)
                                | None -> Some fnone

                            match resVal with
                            | Some r ->
                                target.[curOut] <- (ri, cj, r)
                                loopCol (j + 1UL) nextPtr (curOut + 1)
                            | None -> loopCol (j + 1UL) nextPtr curOut
                        else
                            curPtr, curOut

                    let nextPtr, nextOut = loopCol 0UL ptr outIdx
                    loopRow (i + 1UL) nextPtr nextOut
                else
                    outIdx

            let total = loopRow 0UL 0 0
            let finalArr = if total = target.Length then target else Array.sub target 0 total
            ArrayCOO.Create(coo.nrows, coo.ncols, finalArr)
    | UnaryOp.AllCellsIndexed f ->
        let totalCells = int (uint64 coo.nrows * uint64 coo.ncols)
        let target = Array.zeroCreate totalCells

        let rec loopRow i ptr outIdx =
            if i < uint64 coo.nrows then
                let ri = i * 1UL<rowindex>

                let rec loopCol j curPtr curOut =
                    if j < uint64 coo.ncols then
                        let cj = j * 1UL<colindex>

                        let v, nextPtr =
                            if curPtr < coo.list.Length then
                                let (ei, ej, ev) = coo.list.[curPtr]

                                if ei = ri && ej = cj then
                                    Some ev, curPtr + 1
                                else
                                    None, curPtr
                            else
                                None, curPtr

                        match f ri cj v with
                        | Some r ->
                            target.[curOut] <- (ri, cj, r)
                            loopCol (j + 1UL) nextPtr (curOut + 1)
                        | None -> loopCol (j + 1UL) nextPtr curOut
                    else
                        curPtr, curOut

                let nextPtr, nextOut = loopCol 0UL ptr outIdx
                loopRow (i + 1UL) nextPtr nextOut
            else
                outIdx

        let total = loopRow 0UL 0 0
        let finalArr = if total = target.Length then target else Array.sub target 0 total
        ArrayCOO.Create(coo.nrows, coo.ncols, finalArr)

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
                    let totalCells = int (uint64 nrows * uint64 ncols)
                    let target = Array.zeroCreate totalCells

                    let rec loopRow i p1 p2 outIdx =
                        if i < uint64 nrows then
                            let ri = i * 1UL<rowindex>

                            let rec loopCol j curP1 curP2 curOut =
                                if j < uint64 ncols then
                                    let cj = j * 1UL<colindex>

                                    let v1, nextP1 =
                                        if curP1 < coo1.list.Length then
                                            let (ei, ej, ev) = coo1.list.[curP1]

                                            if ei = ri && ej = cj then
                                                Some ev, curP1 + 1
                                            else
                                                None, curP1
                                        else
                                            None, curP1

                                    let v2, nextP2 =
                                        if curP2 < coo2.list.Length then
                                            let (ei, ej, ev) = coo2.list.[curP2]

                                            if ei = ri && ej = cj then
                                                Some ev, curP2 + 1
                                            else
                                                None, curP2
                                        else
                                            None, curP2

                                    match f v1 v2 with
                                    | Some r ->
                                        target.[curOut] <- (ri, cj, r)
                                        loopCol (j + 1UL) nextP1 nextP2 (curOut + 1)
                                    | None -> loopCol (j + 1UL) nextP1 nextP2 curOut
                                else
                                    curP1, curP2, curOut

                            let nextP1, nextP2, nextOut = loopCol 0UL p1 p2 outIdx
                            loopRow (i + 1UL) nextP1 nextP2 nextOut
                        else
                            outIdx

                    let total = loopRow 0UL 0 0 0
                    if total = target.Length then target else Array.sub target 0 total
            | BinaryOp.AllCellsIndexed f ->
                let totalCells = int (uint64 nrows * uint64 ncols)
                let target = Array.zeroCreate totalCells

                let rec loopRow i p1 p2 outIdx =
                    if i < uint64 nrows then
                        let ri = i * 1UL<rowindex>

                        let rec loopCol j curP1 curP2 curOut =
                            if j < uint64 ncols then
                                let cj = j * 1UL<colindex>

                                let v1, nextP1 =
                                    if curP1 < coo1.list.Length then
                                        let (ei, ej, ev) = coo1.list.[curP1]

                                        if ei = ri && ej = cj then
                                            Some ev, curP1 + 1
                                        else
                                            None, curP1
                                    else
                                        None, curP1

                                let v2, nextP2 =
                                    if curP2 < coo2.list.Length then
                                        let (ei, ej, ev) = coo2.list.[curP2]

                                        if ei = ri && ej = cj then
                                            Some ev, curP2 + 1
                                        else
                                            None, curP2
                                    else
                                        None, curP2

                                match f ri cj v1 v2 with
                                | Some r ->
                                    target.[curOut] <- (ri, cj, r)
                                    loopCol (j + 1UL) nextP1 nextP2 (curOut + 1)
                                | None -> loopCol (j + 1UL) nextP1 nextP2 curOut
                            else
                                curP1, curP2, curOut

                        let nextP1, nextP2, nextOut = loopCol 0UL p1 p2 outIdx
                        loopRow (i + 1UL) nextP1 nextP2 nextOut
                    else
                        outIdx

                let total = loopRow 0UL 0 0 0
                if total = target.Length then target else Array.sub target 0 total
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
    : Result<ArrayCOO<'c>, Error> =
    if uint64 m1.ncols <> uint64 m2.nrows then
        Error Error.InconsistentSizeOfArguments
    else
        let checkMultCondition (arr: COOEntry<'v>[]) getL getR =
            let rec loop i =
                if i < arr.Length then
                    let (_, _, v) = arr.[i]
                    if op_mult (getL v) (getR v) = None then
                        loop (i + 1)
                    else
                        false
                else
                    true
            loop 0

        let canOptimize =
            op_mult None None = None
            && checkMultCondition m1.list (fun v -> Some v) (fun _ -> None)
            && checkMultCondition m2.list (fun _ -> None) (fun v -> Some v)

        let generalResult () =
            let m1Map =
                m1.list |> Array.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofArray

            let m2Map =
                m2.list |> Array.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofArray

            let kCount = uint64 m1.ncols
            let totalCells = int (uint64 m1.nrows * uint64 m2.ncols)
            let result = Array.zeroCreate totalCells

            let rec loopRow i outIdx =
                if i < uint64 m1.nrows then
                    let ri = i * 1UL<rowindex>

                    let rec loopCol j curOut =
                        if j < uint64 m2.ncols then
                            let cj = j * 1UL<colindex>

                            let rec loopK k acc =
                                if k < kCount then
                                    let key1 = (ri, k * 1UL<colindex>)
                                    let key2 = (k * 1UL<rowindex>, cj)
                                    let acc' = op_add acc (op_mult (Map.tryFind key1 m1Map) (Map.tryFind key2 m2Map))
                                    loopK (k + 1UL) acc'
                                else
                                    acc

                            match loopK 0UL None with
                            | Some value ->
                                result.[curOut] <- (ri, cj, value)
                                loopCol (j + 1UL) (curOut + 1)
                            | None -> loopCol (j + 1UL) curOut
                        else
                            curOut

                    let nextOut = loopCol 0UL outIdx
                    loopRow (i + 1UL) nextOut
                else
                    outIdx

            let total = loopRow 0UL 0
            let finalArr = if total = result.Length then result else Array.sub result 0 total
            ArrayCOO.Create(m1.nrows, m2.ncols, finalArr)

        if canOptimize then
            let n1 = m1.list.Length
            let n2 = m2.list.Length
            let numRows = int (uint64 m2.nrows)

            let rowOffsets2 = Array.zeroCreate (numRows + 1)

            let rec countOffsets2 idx =
                if idx < n2 then
                    let (r, _, _) = m2.list.[idx]
                    let rInt = int (uint64 r)
                    rowOffsets2.[rInt + 1] <- rowOffsets2.[rInt + 1] + 1
                    countOffsets2 (idx + 1)

            countOffsets2 0

            let rec prefixSum r =
                if r <= numRows then
                    rowOffsets2.[r] <- rowOffsets2.[r] + rowOffsets2.[r - 1]
                    prefixSum (r + 1)

            prefixSum 1

            let rec countPairs idx acc =
                if idx < n1 then
                    let (_, k, _) = m1.list.[idx]
                    let kInt = int (uint64 k)
                    let count = rowOffsets2.[kInt + 1] - rowOffsets2.[kInt]
                    countPairs (idx + 1) (acc + count)
                else
                    acc

            let totalPairs = countPairs 0 0
            let products = Array.zeroCreate totalPairs

            let rec emitProducts idx writePos =
                if idx < n1 then
                    let (i, k, v1) = m1.list.[idx]
                    let kInt = int (uint64 k)
                    let b = rowOffsets2.[kInt]
                    let e = rowOffsets2.[kInt + 1]

                    let rec loopQ q curWrite =
                        if q < e then
                            let (_, j, v2) = m2.list.[q]

                            match op_mult (Some v1) (Some v2) with
                            | Some prod ->
                                products.[curWrite] <- (i, j, prod)
                                loopQ (q + 1) (curWrite + 1)
                            | None -> loopQ (q + 1) curWrite
                        else
                            curWrite

                    let nextWrite = loopQ b writePos
                    emitProducts (idx + 1) nextWrite
                else
                    writePos

            let actualProductsCount = emitProducts 0 0

            let effectiveProducts =
                if actualProductsCount = products.Length then
                    products
                else
                    Array.sub products 0 actualProductsCount

            let canMerge =
                let rec checkAdd i =
                    if i < effectiveProducts.Length then
                        let (_, _, v) = effectiveProducts.[i]
                        if op_add (Some v) None = Some v && op_add None (Some v) = Some v then
                            checkAdd (i + 1)
                        else
                            false
                    else
                        true
                checkAdd 0

            if canMerge then
                Array.sortInPlaceWith
                    (fun (i1, j1, _) (i2, j2, _) ->
                        let c = compare i1 i2
                        if c <> 0 then c else compare j1 j2)
                    effectiveProducts

                let target = Array.zeroCreate effectiveProducts.Length

                let rec reduceProducts q outIdx =
                    if q < effectiveProducts.Length then
                        let (i, j, v) = effectiveProducts.[q]

                        let rec sumDup qq curSum =
                            if qq < effectiveProducts.Length then
                                let (i', j', v') = effectiveProducts.[qq]

                                if i' = i && j' = j then
                                    sumDup (qq + 1) (op_add curSum (Some v'))
                                else
                                    qq, curSum
                            else
                                qq, curSum

                        let nextQ, finalSum = sumDup (q + 1) (Some v)

                        match finalSum with
                        | Some value ->
                            target.[outIdx] <- (i, j, value)
                            reduceProducts nextQ (outIdx + 1)
                        | None -> reduceProducts nextQ outIdx
                    else
                        outIdx

                let finalCount = reduceProducts 0 0

                let resultArr =
                    if finalCount = target.Length then
                        target
                    else
                        Array.sub target 0 finalCount

                ArrayCOO.Create(m1.nrows, m2.ncols, resultArr) |> Ok
            else
                generalResult () |> Ok
        else
            generalResult () |> Ok
