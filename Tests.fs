module Tests

open Xunit
open HashMap

let init =
    let hashTable: hashMap<int, int> =
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
    let init_size = hm |> getSize
    let size = hm |> add (init_size - 1, 6) |> getSize
    let size2 = hm |> add (init_size - 1, 6) |> add (-1, 10) |> getSize
    Assert.Equal(size * 2, size2)

[<Theory>]
[<InlineData(0)>]
[<InlineData(1)>]
[<InlineData(-1)>]
[<InlineData(5)>]
let ``test delete`` key =
    let hm = init |> delete key
    Assert.False(hasKey key hm)

[<Theory>]
[<InlineData(1, -1)>]
[<InlineData(0, 3)>]
[<InlineData(-1, 4)>]
[<InlineData(5, 5)>]
let ``test filter`` key value =
    let hm = init |> filter (fun n -> (n.value <> value))
    Assert.False(hasKey key hm)

[<Fact>]
let ``test map property`` () =
    let hm = init |> map (fun k v -> $"{k} {v}") |> box
    Assert.True(hm :? hashMap<int, string>)
    Assert.False(hm :? hashMap<int, int>)

[<Fact>]
let ``test fold`` () =
    let hm = init
    Assert.Equal(15, fold (fun state k v -> state + v) 0 hm)
    Assert.Equal(-15, fold (fun state k v -> state - v) 0 hm)
    Assert.Equal(-240, fold (fun state k v -> state * v) 1 hm)

[<Fact>]
let ``test back fold`` () =
    let hm = init
    Assert.Equal(15, backFold (fun state k v -> state + v) 0 hm)
    Assert.Equal(-15, backFold (fun state k v -> state - v) 0 hm)
    Assert.Equal(-240, backFold (fun state k v -> state * v) 1 hm)

[<Fact>]
let ``test property add`` () =
    let hm = init
    let size = hm |> add (5, 6) |> getSize
    let size2 = hm |> add (5, 6) |> add (5, 100) |> getSize
    Assert.Equal(size, size2)

[<Fact>]
let ``test property neutral element`` () =
    let hm = init
    let neutral = create 1
    let sumHm = merge hm neutral
    let otherSumHm = merge neutral hm
    Assert.Equal(hash hm, hash sumHm)
    Assert.Equal(hash hm, hash otherSumHm)

let ``test associativity`` () =
    let hm1 = init
    let hm2 = create 4 |> add (13, 3) |> add (6, 4) |> add (34, 5)
    let hm3 = create 4 |> add (23, 3) |> add (-3, 4) |> add (4, 5)
    let merge12 = merge hm1 hm2
    let merge23 = merge hm2 hm3
    Assert.Equal(hash (merge merge12 hm3), hash (merge hm1 merge23))
