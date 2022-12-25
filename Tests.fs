module Tests

open Microsoft.FSharp.Core
open Xunit
open FsCheck.Xunit
open HashMap

let initHm =
    let hashTable: HashMap<int, int> =
        create 4
        |> add (0, 1)
        |> add (1, -1)
        |> add (3, 4)
        |> add (0, 3)
        |> add (5, 5)
        |> add (-1, 6)
        |> add (-1, 4)

    hashTable

[<Theory>]
[<InlineData(4)>]
[<InlineData(40)>]
[<InlineData(12)>]
[<InlineData(7)>]
let ``test resize`` n =
    let hm = create n
    let initSize = hm |> getSize
    let size = hm |> add (initSize - 1, 6) |> getSize
    let size2 = hm |> add (initSize - 1, 6) |> add (-1, 10) |> getSize
    Assert.Equal(size * 2, size2)

[<Theory>]
[<InlineData(0)>]
[<InlineData(1)>]
[<InlineData(-1)>]
[<InlineData(5)>]
let ``test delete`` key =
    let hm = initHm |> delete key
    Assert.False(hasKey key hm)

[<Theory>]
[<InlineData(1, -1)>]
[<InlineData(0, 3)>]
[<InlineData(-1, 4)>]
[<InlineData(5, 5)>]
let ``test filter`` key value =
    let hm = initHm |> filter (fun n -> (n.value <> value))
    Assert.False(hasKey key hm)

[<Property>]
let ``test map property`` (data: (int * int) list) =
    let hm = init 4 data |> map (fun k v -> $"{k} {v}") |> box
    hm :? HashMap<int, string>

[<Fact>]
let ``test fold`` () =
    let hm = initHm
    Assert.Equal(15, fold (fun state k v -> state + v) 0 hm)
    Assert.Equal(-15, fold (fun state k v -> state - v) 0 hm)
    Assert.Equal(-240, fold (fun state k v -> state * v) 1 hm)

[<Fact>]
let ``test back fold`` () =
    let hm = initHm
    Assert.Equal(15, backFold (fun state k v -> state + v) 0 hm)
    Assert.Equal(-15, backFold (fun state k v -> state - v) 0 hm)
    Assert.Equal(-240, backFold (fun state k v -> state * v) 1 hm)

[<Property>]
let ``test property add`` (node: (int * int), data: (int * int) list) =
    let hm = init 2 data
    let (k, v) = node
    let addedValue = hm |> add node |> get k |> Option.get
    addedValue = v

[<Property>]
let ``test property neutral element`` (data: (int * int) list) =
    let hm = init 4 data
    let neutral = create 1
    let sumHmHash = merge hm neutral
    compare sumHmHash hm

[<Property>]
let ``test associativity`` (data1: (int * int) list, data2: (int * int) list, data3: (int * int) list) =
    let hm1 = init 2 data1
    let hm2 = init 2 data2
    let hm3 = init 2 data3
    let merge12 = merge hm1 hm2
    let merge23 = merge hm2 hm3
    compare (merge merge12 hm3) (merge hm1 merge23)
