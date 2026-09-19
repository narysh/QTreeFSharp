module COO

open Common
open Matrix

let private range (count: uint64) =
    if count = 0UL then [] else [ 0UL .. count - 1UL ]

let cooGet
    (coo: CoordinateList<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>)
    : Result<option<'a>, Error> =
    if uint64 rowindex >= uint64 coo.nrows || uint64 colindex >= uint64 coo.ncols then
        Error Error.InvalidElementIndex
    else
        match coo.list |> List.tryFind (fun (i, j, _) -> i = rowindex && j = colindex) with
        | Some(_, _, value) -> Ok(Some value)
        | None -> Ok None

let cooUpdate
    (coo: CoordinateList<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>, value: 'a)
    : Result<CoordinateList<'a>, Error> =
    if uint64 rowindex >= uint64 coo.nrows || uint64 colindex >= uint64 coo.ncols then
        Error Error.InvalidElementIndex
    else
        let mutable acc = []
        let mutable rest = coo.list
        let mutable inserted = false

        while rest <> [] && not inserted do
            let (i, j, v) = rest.Head

            if i = rowindex && j = colindex then
                acc <- (rowindex, colindex, value) :: acc
                rest <- rest.Tail
                inserted <- true
            elif rowindex < i || (rowindex = i && colindex < j) then
                acc <- (rowindex, colindex, value) :: acc
                inserted <- true
            else
                acc <- (i, j, v) :: acc
                rest <- rest.Tail

        if not inserted then
            acc <- (rowindex, colindex, value) :: acc

        while rest <> [] do
            let entry = rest.Head
            acc <- entry :: acc
            rest <- rest.Tail

        Ok(CoordinateList(coo.nrows, coo.ncols, List.rev acc))


let private applyBinary
    (op: BinaryOp<'a, 'b, 'c>)
    (i: uint64<rowindex>)
    (j: uint64<colindex>)
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
        | Some a, Some b -> f i j a b
        | _ -> None
    | BinaryOp.AllCells f -> f v1 v2
    | BinaryOp.AllCellsIndexed f -> f i j v1 v2
    | BinaryOp.AtLeastOneValue f ->
        match v1, v2 with
        | Some a, Some b -> f (AtLeastOne.Both(a, b))
        | Some a, None -> f (AtLeastOne.Left a)
        | None, Some b -> f (AtLeastOne.Right b)
        | None, None -> None
    | BinaryOp.AtLeastOneValueIndexed f ->
        match v1, v2 with
        | Some a, Some b -> f i j (AtLeastOne.Both(a, b))
        | Some a, None -> f i j (AtLeastOne.Left a)
        | None, Some b -> f i j (AtLeastOne.Right b)
        | None, None -> None
    | BinaryOp.LeftValuesOnly f ->
        match v1 with
        | Some a -> f a v2
        | None -> None
    | BinaryOp.LeftValuesOnlyIndexed f ->
        match v1 with
        | Some a -> f i j a v2
        | None -> None

let private cooMapInner (coo: CoordinateList<'a>) (op: UnaryOp<'a, 'b>) : CoordinateList<'b> =
    let result =
        match op with
        | UnaryOp.ValuesOnly f ->
            coo.list
            |> List.choose (fun (i, j, v) -> f v |> Option.map (fun r -> (i, j, r)))
        | UnaryOp.ValuesOnlyIndexed f ->
            coo.list
            |> List.choose (fun (i, j, v) -> f i j v |> Option.map (fun r -> (i, j, r)))
        | UnaryOp.AllCells f ->
            match f None with
            | None ->
                coo.list
                |> List.choose (fun (i, j, v) -> f (Some v) |> Option.map (fun r -> (i, j, r)))
            | Some fnone ->
                let lookup = coo.list |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList

                [ for i in range (uint64 coo.nrows) do
                      let ri = i * 1UL<rowindex>

                      for j in range (uint64 coo.ncols) do
                          let cj = j * 1UL<colindex>

                          let res =
                              match Map.tryFind (ri, cj) lookup with
                              | Some value -> f (Some value)
                              | None -> Some fnone

                          match res with
                          | Some value -> yield (ri, cj, value)
                          | None -> () ]
        | UnaryOp.AllCellsIndexed f ->
            let mutable rest = coo.list

            [ for i in range (uint64 coo.nrows) do
                  let ri = i * 1UL<rowindex>

                  for j in range (uint64 coo.ncols) do
                      let cj = j * 1UL<colindex>

                      let value =
                          match rest with
                          | (ei, ej, ev) :: tail when ei = ri && ej = cj ->
                              rest <- tail
                              Some ev
                          | _ -> None

                      match f ri cj value with
                      | Some value -> yield (ri, cj, value)
                      | None -> () ]

    CoordinateList(coo.nrows, coo.ncols, result)

let private mergeBinary (l1: COOEntry<'a> list) (l2: COOEntry<'b> list) (op: BinaryOp<'a, 'b, 'c>) : COOEntry<'c> list =
    let mutable acc = []
    let mutable rest1 = l1
    let mutable rest2 = l2

    let emit i j v1 v2 =
        match applyBinary op i j v1 v2 with
        | Some r -> acc <- (i, j, r) :: acc
        | None -> ()

    while rest1 <> [] || rest2 <> [] do
        match rest1, rest2 with
        | [], [] -> ()
        | (i, j, v1) :: t1, [] ->
            emit i j (Some v1) None
            rest1 <- t1
        | [], (i, j, v2) :: t2 ->
            emit i j None (Some v2)
            rest2 <- t2
        | (i1, j1, v1) :: t1, (i2, j2, v2) :: t2 ->
            if i1 = i2 && j1 = j2 then
                emit i1 j1 (Some v1) (Some v2)
                rest1 <- t1
                rest2 <- t2
            elif (i1, j1) < (i2, j2) then
                emit i1 j1 (Some v1) None
                rest1 <- t1
            else
                emit i2 j2 None (Some v2)
                rest2 <- t2

    List.rev acc

let private cooMap2Inner
    (coo1: CoordinateList<'a>)
    (coo2: CoordinateList<'b>)
    (op: BinaryOp<'a, 'b, 'c>)
    : Result<CoordinateList<'c>, Error> =
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
                    let lookup1 = coo1.list |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList

                    let lookup2 = coo2.list |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList

                    [ for i in range (uint64 nrows) do
                          let ri = i * 1UL<rowindex>

                          for j in range (uint64 ncols) do
                              let cj = j * 1UL<colindex>

                              match f (Map.tryFind (ri, cj) lookup1) (Map.tryFind (ri, cj) lookup2) with
                              | Some value -> yield (ri, cj, value)
                              | None -> () ]
            | BinaryOp.AllCellsIndexed f ->
                let mutable rest1 = coo1.list
                let mutable rest2 = coo2.list

                [ for i in range (uint64 nrows) do
                      let ri = i * 1UL<rowindex>

                      for j in range (uint64 ncols) do
                          let cj = j * 1UL<colindex>

                          let v1 =
                              match rest1 with
                              | (ei, ej, ev) :: tail when ei = ri && ej = cj ->
                                  rest1 <- tail
                                  Some ev
                              | _ -> None

                          let v2 =
                              match rest2 with
                              | (ei, ej, ev) :: tail when ei = ri && ej = cj ->
                                  rest2 <- tail
                                  Some ev
                              | _ -> None

                          match f ri cj v1 v2 with
                          | Some value -> yield (ri, cj, value)
                          | None -> () ]
            | _ -> mergeBinary coo1.list coo2.list op

        CoordinateList(nrows, ncols, result) |> Ok

let cooMap (coo: CoordinateList<'a>) f = cooMapInner coo (UnaryOp.AllCells f)

let cooMapValues (coo: CoordinateList<'a>) f = cooMapInner coo (UnaryOp.ValuesOnly f)

let cooMapi (coo: CoordinateList<'a>) f =
    cooMapInner coo (UnaryOp.AllCellsIndexed f)

let cooMapiValues (coo: CoordinateList<'a>) f =
    cooMapInner coo (UnaryOp.ValuesOnlyIndexed f)

let cooMap2 (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCells f)

let cooMap2Values (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.ValuesOnly f)

let cooMap2AllCells (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCells f)

let cooMap2AtLeastOne (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AtLeastOneValue f)

let cooMap2LeftValues (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.LeftValuesOnly f)

let cooMap2i (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCellsIndexed f)

let cooMap2iValues (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.ValuesOnlyIndexed f)

let cooMap2iAllCells (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCellsIndexed f)

let cooMap2iAtLeastOne (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AtLeastOneValueIndexed f)

let cooMap2iLeftValues (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.LeftValuesOnlyIndexed f)

let mxmcoo
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (m1: CoordinateList<'a>)
    (m2: CoordinateList<'b>)
    =
    if uint64 m1.ncols <> uint64 m2.nrows then
        Error Error.InconsistentSizeOfArguments
    else
        let firstA = m1.list |> List.tryHead |> Option.map (fun (_, _, v) -> v)
        let firstB = m2.list |> List.tryHead |> Option.map (fun (_, _, v) -> v)

        let canOptimize =
            let noneNone = op_mult None None = None

            let multSomeNone =
                match firstA with
                | Some v -> op_mult (Some v) None = None
                | None -> noneNone

            let multNoneSome =
                match firstB with
                | Some v -> op_mult None (Some v) = None
                | None -> noneNone

            let addNoneSome =
                match firstA with
                | Some v -> op_add (Some v) None = Some v
                | None -> noneNone

            let addSomeNone =
                match firstB with
                | Some v -> op_add None (Some v) = Some v
                | None -> noneNone

            noneNone && multSomeNone && multNoneSome && addNoneSome && addSomeNone

        if canOptimize then
            let m1ByRow = m1.list |> List.groupBy (fun (i, _, _) -> i) |> Map.ofList
            let m2ByRow = m2.list |> List.groupBy (fun (k, _, _) -> k) |> Map.ofList

            let result =
                [ for KeyValue(i, m1Entries) in m1ByRow do
                      for (_, k, v1) in m1Entries do
                          let kAsRow = uint64 k * 1UL<rowindex>

                          match m2ByRow |> Map.tryFind kAsRow with
                          | Some m2Entries ->
                              for (_, j, v2) in m2Entries do
                                  match op_mult (Some v1) (Some v2) with
                                  | Some product -> yield (i, j, product)
                                  | None -> ()
                          | None -> () ]

            let grouped =
                result
                |> List.groupBy (fun (i, j, _) -> (i, j))
                |> List.map (fun ((i, j), entries) ->
                    let sum = entries |> List.map (fun (_, _, v) -> Some v) |> List.reduce op_add
                    (i, j, sum))
                |> List.choose (fun (i, j, v) -> v |> Option.map (fun v -> (i, j, v)))
                |> List.sortBy (fun (i, j, _) -> (i, j))

            CoordinateList(m1.nrows, m2.ncols, grouped) |> Ok
        else
            let m1Map = m1.list |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList
            let m2Map = m2.list |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList
            let kCount = uint64 m1.ncols

            let result =
                [ for i in range (uint64 m1.nrows) do
                      let ri = i * 1UL<rowindex>

                      for j in range (uint64 m2.ncols) do
                          let cj = j * 1UL<colindex>

                          let products =
                              [ for k in range kCount do
                                    let a = m1Map |> Map.tryFind (ri, k * 1UL<colindex>)
                                    let b = m2Map |> Map.tryFind (k * 1UL<rowindex>, cj)
                                    yield op_mult a b ]

                          let sum = products |> List.fold (fun acc p -> op_add acc p) None

                          match sum with
                          | Some value -> yield (ri, cj, value)
                          | None -> () ]

            CoordinateList(m1.nrows, m2.ncols, result) |> Ok
