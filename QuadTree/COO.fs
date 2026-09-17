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


let cooMap (coo: CoordinateList<'a>) f =
    let updatedList = coo.list |> List.map (fun (i, j, v) -> (i, j, f (Some v)))

    let result =
        match f None with
        | None ->
            updatedList
            |> List.choose (fun (i, j, v) -> v |> Option.map (fun v -> (i, j, v)))
        | Some fnone ->
            let lookup =
                updatedList
                |> List.map (fun (i, j, v) -> ((i, j), v))
                |> Map.ofList

            [ for i in range (uint64 coo.nrows) do
                  let ri = i * 1UL<rowindex>

                  for j in range (uint64 coo.ncols) do
                      let cj = j * 1UL<colindex>

                      match Map.tryFind (ri, cj) lookup with
                      | Some(Some value) -> yield (ri, cj, value)
                      | Some None -> ()
                      | None -> yield (ri, cj, fnone) ]

    CoordinateList(coo.nrows, coo.ncols, result)

let cooMapi (coo: CoordinateList<'a>) f =
    let lookup =
        coo.list
        |> List.map (fun (i, j, v) -> ((i, j), v))
        |> Map.ofList

    let result =
        [ for i in range (uint64 coo.nrows) do
              let ri = i * 1UL<rowindex>

              for j in range (uint64 coo.ncols) do
                  let cj = j * 1UL<colindex>

                  let res = f ri cj (Map.tryFind (ri, cj) lookup)

                  match res with
                  | Some value -> yield (ri, cj, value)
                  | None -> () ]

    CoordinateList(coo.nrows, coo.ncols, result)

let cooMap2 (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    let mutable acc = []
    let mutable l1 = coo1.list
    let mutable l2 = coo2.list

    while l1 <> [] || l2 <> [] do
        match l1, l2 with
        | [], [] -> ()
        | (i1, j1, v1) :: t1, [] ->
            let r = f (Some v1) None
            acc <- (i1, j1, r) :: acc
            l1 <- t1
        | [], (i2, j2, v2) :: t2 ->
            let r = f None (Some v2)
            acc <- (i2, j2, r) :: acc
            l2 <- t2
        | (i1, j1, v1) :: t1, (i2, j2, v2) :: t2 ->
            if i1 = i2 && j1 = j2 then
                let r = f (Some v1) (Some v2)
                acc <- (i1, j1, r) :: acc
                l1 <- t1
                l2 <- t2
            elif (i1, j1) < (i2, j2) then
                let r = f (Some v1) None
                acc <- (i1, j1, r) :: acc
                l1 <- t1
            else
                let r = f None (Some v2)
                acc <- (i2, j2, r) :: acc
                l2 <- t2

    let updatedList = List.rev acc

    let result =
        match f None None with
        | None ->
            updatedList
            |> List.choose (fun (i, j, v) -> v |> Option.map (fun v -> (i, j, v)))
        | Some fnone ->
            let lookup =
                updatedList
                |> List.map (fun (i, j, v) -> ((i, j), v))
                |> Map.ofList

            [ for i in range (uint64 coo1.nrows) do
                  let ri = i * 1UL<rowindex>

                  for j in range (uint64 coo1.ncols) do
                      let cj = j * 1UL<colindex>

                      match Map.tryFind (ri, cj) lookup with
                      | Some(Some value) -> yield (ri, cj, value)
                      | Some None -> ()
                      | None -> yield (ri, cj, fnone) ]

    CoordinateList(coo1.nrows, coo1.ncols, result)

let cooMap2i (coo1: CoordinateList<'a>) (coo2: CoordinateList<'b>) f =
    let mutable acc = []
    let mutable l1 = coo1.list
    let mutable l2 = coo2.list

    while l1 <> [] || l2 <> [] do
        match l1, l2 with
        | [], [] -> ()
        | (i1, j1, v1) :: t1, [] ->
            let r = f i1 j1 (Some v1) None
            acc <- (i1, j1, r) :: acc
            l1 <- t1
        | [], (i2, j2, v2) :: t2 ->
            let r = f i2 j2 None (Some v2)
            acc <- (i2, j2, r) :: acc
            l2 <- t2
        | (i1, j1, v1) :: t1, (i2, j2, v2) :: t2 ->
            if i1 = i2 && j1 = j2 then
                let r = f i1 j1 (Some v1) (Some v2)
                acc <- (i1, j1, r) :: acc
                l1 <- t1
                l2 <- t2
            elif (i1, j1) < (i2, j2) then
                let r = f i1 j1 (Some v1) None
                acc <- (i1, j1, r) :: acc
                l1 <- t1
            else
                let r = f i2 j2 None (Some v2)
                acc <- (i2, j2, r) :: acc
                l2 <- t2

    let result =
        List.rev acc
        |> List.choose (fun (i, j, v) -> v |> Option.map (fun v -> (i, j, v)))

    CoordinateList(coo1.nrows, coo1.ncols, result)

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
