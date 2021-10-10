module Util

let count<'T when 'T: equality> (lst: 'T list) (target: 'T) =
    lst |> List.fold (fun acc item -> if item = target then acc + 1 else acc) 0
