module Graph.BFS

open Common
open Result

type Error =
    | NewFrontierCalculationProblem of LinearAlgebra.Error
    | FrontierCalculationProblem of Vector.Error
    | VisitedCalculationProblem of Vector.Error

let bfs
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: uint64<Vector.index> * 'c -> uint64<Matrix.rowindex> * uint64<Matrix.colindex> * 'b -> Option<'c>)
    (initVisited: uint64<Vector.index> -> Option<'c> -> Option<'c>)
    (graph: Matrix.SparseMatrix<'b>)
    (startVertices: Vector.SparseVector<'c>)
    =
    let initialVisited = Vector.mapi startVertices initVisited

    let rec inner (frontier: Vector.SparseVector<'c>) (visited: Vector.SparseVector<'c>) =
        if frontier.nvals > 0UL<nvals> then
            resultM {
                let! new_frontier =
                    LinearAlgebra.vxmi_values op_add op_mult frontier graph
                    |> Result.mapError NewFrontierCalculationProblem

                let! frontier =
                    Vector.mask new_frontier visited (fun x -> x.IsNone)
                    |> Result.mapError FrontierCalculationProblem

                let! visited =
                    Vector.map2 visited frontier (fun oldVal newVal ->
                        match oldVal with
                        | Some _ -> oldVal
                        | None -> newVal)
                    |> Result.mapError VisitedCalculationProblem

                return! inner frontier visited
            }
        else
            Ok visited

    inner startVertices initialVisited

let bfs_level graph startVertices =
    let op_add x y =
        match (x, y) with
        | Some(v), _
        | _, Some(v) -> Some(v)
        | _ -> None

    let op_mult (_, vp) (_, _, _) = Some(vp + 1UL)

    let initVisited _ v = v |> Option.map (fun _ -> 0UL)

    let frontier0 =
        Vector.mapi startVertices (fun _ v -> v |> Option.map (fun _ -> 0UL))

    bfs op_add op_mult initVisited graph frontier0

let bfs_parent graph startVertices =
    let op_add x y =
        match (x, y) with
        | Some(v), _
        | _, Some(v) -> Some(v)
        | _ -> None

    let op_mult (vi, _) (_, _, _) = Some(uint64 vi)

    let initVisited i v = v |> Option.map (fun _ -> uint64 i)

    bfs op_add op_mult initVisited graph startVertices
