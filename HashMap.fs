module HashMap

type Node<'K, 'V> =
    struct
        val key: 'K
        val value: 'V
        val isDeleted: bool

        new(_key: 'K, _value: 'V, _isDeleted: bool) =
            { key = _key
              value = _value
              isDeleted = _isDeleted }

        new(_key: 'K, _value: 'V) =
            { key = _key
              value = _value
              isDeleted = false }
    end

type HashMap<'K, 'V> = Option<Node<'K, 'V>>[]

let create<'K, 'V> (count: int) : HashMap<'K, 'V> = Array.create count Option.None

let getLength (hashMap: HashMap<'K, 'V>) = Array.length hashMap
let private getNext i = i + 1

let getIndex h (hashMap: HashMap<'K, 'V>) : int =
    let index = h &&& (getLength hashMap - 1)
    index


let rec private addRec (i: int) (key: 'K, v: 'V) (hashTable: HashMap<'K, 'V>) =
    let size = hashTable.Length

    if (i < size) then
        match hashTable[i] with
        | Some nd when nd.isDeleted || ((not nd.isDeleted) && (nd.key = key)) ->
            hashTable[i] <- Option.Some(Node(key, v))
            hashTable
        | Some nd when (not nd.isDeleted) && (nd.key <> key) -> addRec (getNext i) (key, v) hashTable
        | _ ->
            hashTable[i] <- Option.Some(Node(key, v))
            hashTable
    else
        let newHM = resize hashTable
        addRec (getIndex (hash key) newHM) (key, v) newHM

and resize (hashMap: HashMap<'K, 'V>) : HashMap<'K, 'V> =
    let size = getLength hashMap
    let newHashMap = create<'K, 'V> (size * 2)

    let rec transfer i (hMap: HashMap<'K, 'V>) : HashMap<'K, 'V> =
        if i < size then
            match hashMap[i] with
            | Some nd -> transfer (i + 1) (addRec (getIndex (hash nd.key) hMap) (nd.key, nd.value) hMap)
            | _ -> transfer (i + 1) hMap
        else
            hMap

    transfer 0 newHashMap

let add (key: 'K, v: 'V) (hashTable: HashMap<'K, 'V>) =
    let innerMap = Array.copy hashTable
    addRec (getIndex (hash key) innerMap) (key, v) innerMap

let init n (data: _ list) =
    let hm = create n

    let rec dataAdder i newData =
        if i < data.Length then
            dataAdder (i + 1) (add (data[i]) newData)
        else
            newData

    dataAdder 0 hm

let rec private findIndexByKeyRec i (key: 'K) (hashMap: HashMap<'K, 'V>) =
    let size = getLength hashMap

    if i < size then
        match hashMap[i] with
        | Some nd when (not nd.isDeleted) && (nd.key <> key) -> findIndexByKeyRec (getNext i) key hashMap
        | Some nd when (not nd.isDeleted) && (nd.key = key) -> i
        | _ -> -1
    else
        -1

let rec findIndexByKey (key: 'K) (hashMap: HashMap<'K, 'V>) =
    let hash = hash key
    let startIndex = getIndex hash hashMap
    findIndexByKeyRec startIndex key hashMap

let get (key: 'K) (hashMap: HashMap<'K, 'V>) =
    let index = findIndexByKey key hashMap

    if index <> -1 then
        Option.Some hashMap[index].Value
    else
        Option.None

let public markAsDeleted (deleteNd: Node<'K, 'V>) (hashMap: HashMap<'K, 'V>) : HashMap<'K, 'V> =
    let size = hashMap.Length
    let ndOption = get deleteNd.key hashMap

    if ndOption.IsSome then
        let nd = ndOption.Value
        let i = findIndexByKey nd.key hashMap

        if i = (size - 1) then
            hashMap[i] <- Option.None
        else
            hashMap[i] <- Option.Some(Node<'K, 'V>(deleteNd.key, deleteNd.value, true))

    hashMap


let delete (key: 'K) (hashMap: HashMap<'K, 'V>) =
    let innerHM = Array.copy hashMap
    let ndOption = get key innerHM

    match ndOption with
    | Some ndOption -> markAsDeleted ndOption innerHM
    | _ -> innerHM

let hasKey (key: 'K) (hashMap: HashMap<'K, 'V>) = findIndexByKey key hashMap <> -1


let print (hashMap: HashMap<_, _>) =
    for i in hashMap do
        match i with
        | Some nd when nd.isDeleted -> printfn "deleted"
        | Some nd when not nd.isDeleted -> printfn $"{i.Value.key} {i.Value.value}"
        | _ -> printfn "null"

    hashMap

let filter (f: Node<'K, 'V> -> bool) (hashMap: HashMap<'K, 'V>) =
    let innerMap = Array.copy hashMap

    let rec doFilter (hm: HashMap<'K, 'V>) : HashMap<'K, 'V> =
        let size = hm.Length

        match hm |> Array.truncate 1 with
        | [| ndOption |] when ndOption.IsSome ->
            let nd = ndOption.Value

            if not (f nd) then
                innerMap |> markAsDeleted nd |> ignore

            doFilter hm[1..size]
        | [| ndOption |] when ndOption.IsNone -> doFilter hm[1..size]
        | _ -> innerMap

    doFilter innerMap

let map (f: 'K -> 'V -> 'U) (hashMap: HashMap<'K, 'V>) =
    let size = hashMap.Length
    let innerMap = create<'K, 'U> size

    let rec mapRec i =
        match i with
        | 0 -> innerMap
        | _ ->
            match hashMap[i] with
            | Some nd ->
                innerMap[i] <- Option.Some(Node(nd.key, f nd.key nd.value, nd.isDeleted))
                mapRec (i - 1)
            | _ -> mapRec (i - 1)

    mapRec (size - 1)

let fold (f: 'T -> 'K -> 'V -> 'T) (init: 'T) (hashMap: HashMap<'K, 'V>) : 'T =
    let rec foldRec i res =
        if i < getLength hashMap then
            match hashMap[i] with
            | Some nd when not nd.isDeleted -> foldRec (i + 1) (f res nd.key nd.value)
            | _ -> foldRec (i + 1) res
        else
            res

    foldRec 0 init

let backFold (f: 'T -> 'K -> 'V -> 'T) (init: 'T) (hashMap: HashMap<'K, 'V>) : 'T =
    let size = hashMap.Length

    let rec foldRec i res =
        if i >= 0 then
            match hashMap[i] with
            | Some nd when not nd.isDeleted -> foldRec (i - 1) (f res nd.key nd.value)
            | _ -> foldRec (i - 1) res
        else
            res

    foldRec (size - 1) init

let getSize (hm: HashMap<'K, 'V>) : int = fold (fun size _ _ -> (size + 1)) 0 hm

let merge (hashMap1: HashMap<'K, 'V>) (hashMap2: HashMap<'K, 'V>) : HashMap<'K, 'V> =
    if (getSize hashMap1 = 0) then
        hashMap2
    elif (getSize hashMap2 = 0) then
        hashMap1
    else
        let initSize = max (getLength hashMap1) (getLength hashMap2)
        let sumHm = create initSize

        let rec iterOverHM i (hm: HashMap<'K, 'V>) (resHm: HashMap<'K, 'V>) =
            if i >= 0 then
                match hm[i] with
                | Some nd when not nd.isDeleted -> add (nd.key, nd.value) resHm |> iterOverHM (i - 1) hm
                | Some nd when nd.isDeleted -> iterOverHM (i - 1) hm resHm
                | _ -> iterOverHM (i - 1) hm resHm
            else
                resHm

        sumHm
        |> iterOverHM (getLength hashMap1 - 1) hashMap1
        |> iterOverHM (getLength hashMap2 - 1) hashMap2

let compare (hashMap1: HashMap<'K, 'V>) (hashMap2: HashMap<'K, 'V>) : bool =
    let rec recCompare (i: int) : bool =
        if i < hashMap1.Length then
            let nd = hashMap1[i]

            match nd with
            | Some nd when not nd.isDeleted ->
                let ndOption2 = get nd.key hashMap2
                let cmp = ndOption2.IsSome && ndOption2.Value.value = nd.value
                if not cmp then false else recCompare (i + 1)
            | _ -> recCompare (i + 1)
        else
            true

    if (getSize hashMap1) <> (getSize hashMap2) then
        false
    else
        recCompare 0

let (.=.) (hashMap1: HashMap<'K, 'V>) (hashMap2: HashMap<'K, 'V>) = compare hashMap1 hashMap2
