module COOList

open Common
open Matrix
open COOArray

[<Struct>]
type ListCOO<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val entries: COOEntry<'value> list

    new(_nrows, _ncols, _entries: COOEntry<'value> list) =
        { nrows = _nrows
          ncols = _ncols
          entries = _entries }

    member this.Item
        with get (rowindex: uint64<rowindex>, colindex: uint64<colindex>): Result<option<'value>, Error> =
            match checkBounds (uint64 this.nrows) (uint64 this.ncols) (uint64 rowindex) (uint64 colindex) with
            | Error e -> Error e
            | Ok() ->
                let rec findEntry list =
                    match list with
                    | [] -> None
                    | (i, j, v) :: tail ->
                        if i = rowindex && j = colindex then
                            Some v
                        elif i > rowindex || (i = rowindex && j > colindex) then
                            None
                        else
                            findEntry tail

                Ok(findEntry this.entries)

let cooGet (coo: ListCOO<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>) : Result<option<'a>, Error> =
    coo.[rowindex, colindex]

let fromArray (coo: ArrayCOO<'a>) : ListCOO<'a> =
    ListCOO<'a>(coo.nrows, coo.ncols, Array.toList coo.entries)

let toArray (coo: ListCOO<'a>) : ArrayCOO<'a> =
    ArrayCOO.CreatePresorted(coo.nrows, coo.ncols, Array.ofList coo.entries)

let cooUpdate
    (coo: ListCOO<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>, value: 'a)
    : Result<ListCOO<'a>, Error> =
    match checkBounds (uint64 coo.nrows) (uint64 coo.ncols) (uint64 rowindex) (uint64 colindex) with
    | Error e -> Error e
    | Ok() ->
        let rec loop rest acc inserted =
            match rest with
            | [] ->
                if inserted then
                    List.rev acc
                else
                    List.rev ((rowindex, colindex, value) :: acc)
            | (i, j, v) :: tail ->
                if inserted then
                    loop tail ((i, j, v) :: acc) true
                elif i = rowindex && j = colindex then
                    loop tail ((rowindex, colindex, value) :: acc) true
                elif rowindex < i || (rowindex = i && colindex < j) then
                    loop rest ((rowindex, colindex, value) :: acc) true
                else
                    loop tail ((i, j, v) :: acc) false

        Ok(ListCOO<'a>(coo.nrows, coo.ncols, loop coo.entries [] false))

let private cooMapInner (coo: ListCOO<'a>) (op: UnaryOp<'a, 'b>) : ListCOO<'b> =
    let result =
        match op with
        | UnaryOp.ValuesOnly f ->
            coo.entries
            |> List.choose (fun (i, j, v) -> f v |> Option.map (fun r -> (i, j, r)))
        | UnaryOp.ValuesOnlyIndexed f ->
            coo.entries
            |> List.choose (fun (i, j, v) -> f i j v |> Option.map (fun r -> (i, j, r)))
        | UnaryOp.AllCells f ->
            match f None with
            | None ->
                coo.entries
                |> List.choose (fun (i, j, v) -> f (Some v) |> Option.map (fun r -> (i, j, r)))
            | Some fnone ->
                let rec loopRow i curRest acc =
                    if i < uint64 coo.nrows then
                        let ri = i * 1UL<rowindex>

                        let rec loopCol j r acc' =
                            if j < uint64 coo.ncols then
                                let cj = j * 1UL<colindex>

                                let v, nextR =
                                    match r with
                                    | (ei, ej, ev) :: tail when ei = ri && ej = cj -> Some ev, tail
                                    | _ -> None, r

                                let res =
                                    match v with
                                    | Some value -> f (Some value)
                                    | None -> Some fnone

                                let acc'' =
                                    match res with
                                    | Some value -> (ri, cj, value) :: acc'
                                    | None -> acc'

                                loopCol (j + 1UL) nextR acc''
                            else
                                r, acc'

                        let nextR, nextAcc = loopCol 0UL curRest acc
                        loopRow (i + 1UL) nextR nextAcc
                    else
                        List.rev acc

                loopRow 0UL coo.entries []
        | UnaryOp.AllCellsIndexed f ->
            let rec loopRow i curRest acc =
                if i < uint64 coo.nrows then
                    let ri = i * 1UL<rowindex>

                    let rec loopCol j r acc' =
                        if j < uint64 coo.ncols then
                            let cj = j * 1UL<colindex>

                            let v, nextR =
                                match r with
                                | (ei, ej, ev) :: tail when ei = ri && ej = cj -> Some ev, tail
                                | _ -> None, r

                            let acc'' =
                                match f ri cj v with
                                | Some value -> (ri, cj, value) :: acc'
                                | None -> acc'

                            loopCol (j + 1UL) nextR acc''
                        else
                            r, acc'

                    let nextR, nextAcc = loopCol 0UL curRest acc
                    loopRow (i + 1UL) nextR nextAcc
                else
                    List.rev acc

            loopRow 0UL coo.entries []

    ListCOO<'b>(coo.nrows, coo.ncols, result)

let private mergeBinary (l1: COOEntry<'a> list) (l2: COOEntry<'b> list) (op: BinaryOp<'a, 'b, 'c>) : COOEntry<'c> list =
    let rec loop acc rest1 rest2 =
        match rest1, rest2 with
        | [], [] -> List.rev acc
        | (i, j, v1) :: t1, [] ->
            let acc' =
                match applyBinary op i j (Some v1) None with
                | Some r -> (i, j, r) :: acc
                | None -> acc

            loop acc' t1 []
        | [], (i, j, v2) :: t2 ->
            let acc' =
                match applyBinary op i j None (Some v2) with
                | Some r -> (i, j, r) :: acc
                | None -> acc

            loop acc' [] t2
        | (i1, j1, v1) :: t1, (i2, j2, v2) :: t2 ->
            if i1 = i2 && j1 = j2 then
                let acc' =
                    match applyBinary op i1 j1 (Some v1) (Some v2) with
                    | Some r -> (i1, j1, r) :: acc
                    | None -> acc

                loop acc' t1 t2
            elif i1 < i2 || (i1 = i2 && j1 < j2) then
                let acc' =
                    match applyBinary op i1 j1 (Some v1) None with
                    | Some r -> (i1, j1, r) :: acc
                    | None -> acc

                loop acc' t1 ((i2, j2, v2) :: t2)
            else
                let acc' =
                    match applyBinary op i2 j2 None (Some v2) with
                    | Some r -> (i2, j2, r) :: acc
                    | None -> acc

                loop acc' ((i1, j1, v1) :: t1) t2

    loop [] l1 l2

let private cooMap2Inner
    (coo1: ListCOO<'a>)
    (coo2: ListCOO<'b>)
    (op: BinaryOp<'a, 'b, 'c>)
    : Result<ListCOO<'c>, Error> =
    if uint64 coo1.nrows <> uint64 coo2.nrows || uint64 coo1.ncols <> uint64 coo2.ncols then
        Error Error.InconsistentSizeOfArguments
    else
        let nrows = coo1.nrows
        let ncols = coo1.ncols

        let result =
            match op with
            | BinaryOp.AllCells f ->
                match f None None with
                | None -> mergeBinary coo1.entries coo2.entries op
                | Some _ ->
                    let rec loopRow i curRest1 curRest2 acc =
                        if i < uint64 nrows then
                            let ri = i * 1UL<rowindex>

                            let rec loopCol j r1 r2 acc' =
                                if j < uint64 ncols then
                                    let cj = j * 1UL<colindex>

                                    let v1, nextR1 =
                                        match r1 with
                                        | (ei, ej, ev) :: tail when ei = ri && ej = cj -> Some ev, tail
                                        | _ -> None, r1

                                    let v2, nextR2 =
                                        match r2 with
                                        | (ei, ej, ev) :: tail when ei = ri && ej = cj -> Some ev, tail
                                        | _ -> None, r2

                                    let acc'' =
                                        match f v1 v2 with
                                        | Some value -> (ri, cj, value) :: acc'
                                        | None -> acc'

                                    loopCol (j + 1UL) nextR1 nextR2 acc''
                                else
                                    r1, r2, acc'

                            let nextR1, nextR2, nextAcc = loopCol 0UL curRest1 curRest2 acc
                            loopRow (i + 1UL) nextR1 nextR2 nextAcc
                        else
                            List.rev acc

                    loopRow 0UL coo1.entries coo2.entries []
            | BinaryOp.AllCellsIndexed f ->
                let rec loopRow i curRest1 curRest2 acc =
                    if i < uint64 nrows then
                        let ri = i * 1UL<rowindex>

                        let rec loopCol j r1 r2 acc' =
                            if j < uint64 ncols then
                                let cj = j * 1UL<colindex>

                                let v1, nextR1 =
                                    match r1 with
                                    | (ei, ej, ev) :: tail when ei = ri && ej = cj -> Some ev, tail
                                    | _ -> None, r1

                                let v2, nextR2 =
                                    match r2 with
                                    | (ei, ej, ev) :: tail when ei = ri && ej = cj -> Some ev, tail
                                    | _ -> None, r2

                                let acc'' =
                                    match f ri cj v1 v2 with
                                    | Some value -> (ri, cj, value) :: acc'
                                    | None -> acc'

                                loopCol (j + 1UL) nextR1 nextR2 acc''
                            else
                                r1, r2, acc'

                        let nextR1, nextR2, nextAcc = loopCol 0UL curRest1 curRest2 acc
                        loopRow (i + 1UL) nextR1 nextR2 nextAcc
                    else
                        List.rev acc

                loopRow 0UL coo1.entries coo2.entries []
            | _ -> mergeBinary coo1.entries coo2.entries op

        ListCOO<'c>(nrows, ncols, result) |> Ok

let cooMap (coo: ListCOO<'a>) f = cooMapInner coo (UnaryOp.AllCells f)

let cooMapValues (coo: ListCOO<'a>) f = cooMapInner coo (UnaryOp.ValuesOnly f)

let cooMapi (coo: ListCOO<'a>) f =
    cooMapInner coo (UnaryOp.AllCellsIndexed f)

let cooMapiValues (coo: ListCOO<'a>) f =
    cooMapInner coo (UnaryOp.ValuesOnlyIndexed f)

let cooMap2Values (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.ValuesOnly f)

let cooMap2AllCells (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCells f)

let cooMap2 = cooMap2AllCells

let cooMap2AtLeastOne (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AtLeastOneValue f)

let cooMap2LeftValues (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.LeftValuesOnly f)

let cooMap2iValues (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.ValuesOnlyIndexed f)

let cooMap2iAllCells (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCellsIndexed f)

let cooMap2i = cooMap2iAllCells

let cooMap2iAtLeastOne (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AtLeastOneValueIndexed f)

let cooMap2iLeftValues (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.LeftValuesOnlyIndexed f)

let mxmcoo
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (m1: ListCOO<'a>)
    (m2: ListCOO<'b>)
    =
    if uint64 m1.ncols <> uint64 m2.nrows then
        Error Error.InconsistentSizeOfArguments
    else
        let entries1 = m1.entries
        let entries2 = m2.entries

        let checkMultCondition entries getL getR =
            let rec loop list =
                match list with
                | [] -> true
                | (_, _, v) :: tail ->
                    if Option.isNone (op_mult (getL v) (getR v)) then
                        loop tail
                    else
                        false

            loop entries

        let canOptimize =
            Option.isNone (op_mult None None)
            && Option.isNone (op_add None None)
            && checkMultCondition entries1 (fun v -> Some v) (fun _ -> None)
            && checkMultCondition entries2 (fun _ -> None) (fun v -> Some v)

        let generalResult () =
            let m1Map = entries1 |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList
            let m2Map = entries2 |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList
            let kCount = uint64 m1.ncols

            let rec loopRow i acc =
                if i < uint64 m1.nrows then
                    let ri = i * 1UL<rowindex>

                    let rec loopCol j colAcc =
                        if j < uint64 m2.ncols then
                            let cj = j * 1UL<colindex>

                            let rec loopK k kAcc =
                                if k < kCount then
                                    let a = m1Map |> Map.tryFind (ri, k * 1UL<colindex>)
                                    let b = m2Map |> Map.tryFind (k * 1UL<rowindex>, cj)
                                    loopK (k + 1UL) (op_add kAcc (op_mult a b))
                                else
                                    kAcc

                            let sum = loopK 0UL None

                            let colAcc' =
                                match sum with
                                | Some value -> (ri, cj, value) :: colAcc
                                | None -> colAcc

                            loopCol (j + 1UL) colAcc'
                        else
                            colAcc

                    let rowAcc = loopCol 0UL acc
                    loopRow (i + 1UL) rowAcc
                else
                    List.rev acc

            let result = loopRow 0UL []
            ListCOO<'c>(m1.nrows, m2.ncols, result)

        if canOptimize then
            let m1ByRow = entries1 |> List.groupBy (fun (i, _, _) -> i) |> Map.ofList
            let m2ByRow = entries2 |> List.groupBy (fun (k, _, _) -> k) |> Map.ofList

            let rec emitProducts m1List acc =
                match m1List with
                | [] -> acc
                | (i, k, v1) :: tail ->
                    let kAsRow = uint64 k * 1UL<rowindex>

                    let acc' =
                        match m2ByRow |> Map.tryFind kAsRow with
                        | Some m2Entries ->
                            let rec loopM2 m2List curAcc =
                                match m2List with
                                | [] -> curAcc
                                | (_, j, v2) :: m2Tail ->
                                    match op_mult (Some v1) (Some v2) with
                                    | Some product -> loopM2 m2Tail ((i, j, product) :: curAcc)
                                    | None -> loopM2 m2Tail curAcc

                            loopM2 m2Entries acc
                        | None -> acc

                    emitProducts tail acc'

            let result = emitProducts entries1 []

            let checkAddCondition list =
                let rec loop l =
                    match l with
                    | [] -> true
                    | (_, _, v) :: tail ->
                        if op_add (Some v) None = Some v && op_add None (Some v) = Some v then
                            loop tail
                        else
                            false

                loop list

            let canMerge = checkAddCondition result

            if canMerge then
                let sorted =
                    result
                    |> List.sortWith (fun (i1, j1, _) (i2, j2, _) ->
                        let c = compare i1 i2
                        if c <> 0 then c else compare j1 j2)

                let rec reduceAdjacent acc curKey curSum rest =
                    match rest with
                    | [] ->
                        match curSum with
                        | Some v -> List.rev ((fst curKey, snd curKey, v) :: acc)
                        | None -> List.rev acc
                    | (i, j, v) :: tail ->
                        if (i, j) = curKey then
                            reduceAdjacent acc curKey (op_add curSum (Some v)) tail
                        else
                            let acc' =
                                match curSum with
                                | Some sv -> (fst curKey, snd curKey, sv) :: acc
                                | None -> acc

                            reduceAdjacent acc' (i, j) (Some v) tail

                let grouped =
                    match sorted with
                    | [] -> []
                    | (i, j, v) :: tail -> reduceAdjacent [] (i, j) (Some v) tail

                ListCOO<'c>(m1.nrows, m2.ncols, grouped) |> Ok
            else
                generalResult () |> Ok
        else
            generalResult () |> Ok
