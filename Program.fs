module Program

open HashMap

[<EntryPoint>]
let main _ =
    // Пример использования некоторых функций
    let hashTable: hashMap<int, int> = create 4
    let emptyHM = create 1

    hashTable
    |> add (0, 1)
    |> add (1, -1)
    |> add (3, 4)
    |> add (0, 3)
    |> add (5, 5)
    |> add (-2, 6)
    |> add (6, 3)
    |> delete 6
    |> add (-1, 4)
    |> filter (fun n -> (n.value <> 4))
    |> merge emptyHM
    |> map (fun k v -> ($"{k} {v}"))
    |> print
    |> ignore

    0
