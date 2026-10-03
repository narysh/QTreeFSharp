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

let fromArray (coo: ArrayCOO<'a>) : ListCOO<'a> =
    ListCOO<'a>(coo.nrows, coo.ncols, Array.toList coo.list)

let toArray (coo: ListCOO<'a>) : ArrayCOO<'a> =
    ArrayCOO.Create(coo.nrows, coo.ncols, Array.ofList coo.entries)

let private mapArrayResult f coo = f (toArray coo) |> Result.map fromArray

let cooGet (coo: ListCOO<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>) : Result<option<'a>, Error> =
    validateCOOIndex coo.nrows coo.ncols "rowindex" "colindex" rowindex colindex

    match coo.entries |> List.tryFind (fun (i, j, _) -> i = rowindex && j = colindex) with
    | Some(_, _, value) -> Ok(Some value)
    | None -> Ok None

let cooUpdate
    (coo: ListCOO<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>, value: 'a)
    : Result<ListCOO<'a>, Error> =
    validateCOOIndex coo.nrows coo.ncols "rowindex" "colindex" rowindex colindex

    let mutable acc = []
    let mutable rest = coo.entries
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

    Ok(ListCOO<'a>(coo.nrows, coo.ncols, List.rev acc))

let cooMap (coo: ListCOO<'a>) f =
    COOArray.cooMap (toArray coo) f |> fromArray

let cooMapValues (coo: ListCOO<'a>) f =
    COOArray.cooMapValues (toArray coo) f |> fromArray

let cooMapi (coo: ListCOO<'a>) f =
    COOArray.cooMapi (toArray coo) f |> fromArray

let cooMapiValues (coo: ListCOO<'a>) f =
    COOArray.cooMapiValues (toArray coo) f |> fromArray

let cooMap2 (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    mapArrayResult (fun a -> COOArray.cooMap2 a (toArray coo2) f) coo1

let cooMap2Values (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    mapArrayResult (fun a -> COOArray.cooMap2Values a (toArray coo2) f) coo1

let cooMap2AllCells (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    mapArrayResult (fun a -> COOArray.cooMap2AllCells a (toArray coo2) f) coo1

let cooMap2AtLeastOne (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    mapArrayResult (fun a -> COOArray.cooMap2AtLeastOne a (toArray coo2) f) coo1

let cooMap2LeftValues (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    mapArrayResult (fun a -> COOArray.cooMap2LeftValues a (toArray coo2) f) coo1

let cooMap2i (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    mapArrayResult (fun a -> COOArray.cooMap2i a (toArray coo2) f) coo1

let cooMap2iValues (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    mapArrayResult (fun a -> COOArray.cooMap2iValues a (toArray coo2) f) coo1

let cooMap2iAllCells (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    mapArrayResult (fun a -> COOArray.cooMap2iAllCells a (toArray coo2) f) coo1

let cooMap2iAtLeastOne (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    mapArrayResult (fun a -> COOArray.cooMap2iAtLeastOne a (toArray coo2) f) coo1

let cooMap2iLeftValues (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    mapArrayResult (fun a -> COOArray.cooMap2iLeftValues a (toArray coo2) f) coo1

let mxmcoo
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (m1: ListCOO<'a>)
    (m2: ListCOO<'b>)
    =
    COOArray.mxmcoo op_add op_mult (toArray m1) (toArray m2) |> Result.map fromArray
