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

let getSize (hashMap: HashMap<'K, 'V>) = Array.length hashMap
let private getNext i = i + 1

let getIndex h (hashMap: HashMap<'K, 'V>) : int =
    let index = h &&& (getSize hashMap - 1)
    index


let rec private addRec (i: int) (key: 'K, v: 'V) (hashTable: HashMap<'K, 'V>) =
    let size = hashTable.Length

    if (i < size) then
        let ndOption = hashTable[i]

        if ndOption.IsNone then
            hashTable[i] <- Option.Some(Node (key, v))
            hashTable
        else
            let nd = ndOption.Value

            if nd.isDeleted then
                hashTable[i] <- Option.Some(Node (key, v))
                hashTable
            else if (nd.key <> key) then
                addRec (getNext i) (key, v) hashTable
            else
                hashTable[i] <- Option.Some(Node (key, v))
                hashTable
    else
        let newHM = resize hashTable
        addRec (getIndex (hash key) newHM) (key, v) newHM

and resize (hashMap: HashMap<'K, 'V>) : HashMap<'K, 'V> =
    let size = getSize hashMap
    let newHashMap = create<'K, 'V> (size * 2)

    let rec transfer i (hMap: HashMap<'K, 'V>) : HashMap<'K, 'V> =
        if i < size then
            let ndOption = hashMap[i]

            if ndOption.IsSome then
                let nd = ndOption.Value
                transfer (i + 1) (addRec (getIndex (hash nd.key) hMap) (nd.key, nd.value) hMap)
            else
                transfer (i + 1) hMap
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

let private markAsDeleted i (hashMap: HashMap<'K, 'V>) : HashMap<'K, 'V> =
    let size = hashMap.Length
    let ndOption = hashMap[i]

    if ndOption.IsSome then
        let nd = ndOption.Value

        if i = (size - 1) then
            hashMap[i] <- Option.None
        else
            hashMap[i] <- Option.Some(Node<'K, 'V> (nd.key, nd.value, true))

    hashMap

let rec private findIndexByKey i (key: 'K) (hashMap: HashMap<'K, 'V>) =
    let size = getSize hashMap

    if i < size then
        let nd = hashMap[i]

        if nd.IsNone then
            -1
        else if nd.Value.isDeleted then
            if (nd.Value.key = key) then
                -1
            else
                findIndexByKey (getNext i) key hashMap
        else if key = nd.Value.key then
            i
        else
            findIndexByKey (getNext i) key hashMap
    else
        -1

let delete (key: 'K) (hashMap: HashMap<'K, 'V>) =
    let innerHashMap = Array.copy hashMap
    let hash = hash key
    let index = findIndexByKey (getIndex hash hashMap) key hashMap

    if index >= 0 then
        markAsDeleted index innerHashMap
    else
        hashMap

let hasKey (key: 'K) (hashMap: HashMap<'K, 'V>) =
    let hash = hash key
    let startIndex = getIndex hash hashMap
    printfn $"start index : {startIndex}"
    findIndexByKey startIndex key hashMap <> -1

let get (key: 'K) (hashMap: HashMap<'K, 'V>) =
    let hash = hash key
    let startIndex = getIndex hash hashMap
    printfn $"start index : {startIndex}"
    let index = findIndexByKey startIndex key hashMap

    if index <> -1 then
        Option.Some hashMap[index].Value.value
    else
        Option.None

let print (hashMap: HashMap<_, _>) =
    for i in hashMap do
        if i.IsSome then
            if i.Value.isDeleted then
                printfn "deleted"
            else
                printfn $"{i.Value.key} {i.Value.value}"
        else
            printfn "null"

    hashMap

let filter (f: Node<'K, 'V> -> bool) (hashMap: HashMap<'K, 'V>) =
    let innerMap = Array.copy hashMap
    let size = hashMap.Length

    let rec doFilter i (hm: HashMap<'K, 'V>) : HashMap<'K, 'V> =
        if (i < size) then
            let ndOption = hm[i]

            if ndOption.IsSome then
                if not (f (hm[i].Value)) then
                    markAsDeleted i hm |> doFilter (i + 1)
                else
                    doFilter (i + 1) hm
            else
                doFilter (i + 1) hm
        else
            hm

    doFilter 0 innerMap

let map (f: 'K -> 'V -> 'U) (hashMap: HashMap<'K, 'V>) =
    let size = hashMap.Length
    let innerMap = create<'K, 'U> size

    let rec mapRec i =
        if i < size then
            let ndOption = hashMap[i]

            if ndOption.IsSome then
                let nd = ndOption.Value
                innerMap[i] <- Option.Some(Node (nd.key, f nd.key nd.value, nd.isDeleted))
                mapRec (i + 1)
            else
                mapRec (i + 1)
        else
            innerMap

    mapRec 0

let fold (f: 'T -> 'K -> 'V -> 'T) (init: 'T) (hashMap: HashMap<'K, 'V>) : 'T =
    let size = hashMap.Length

    let rec foldRec i res =
        if i < size then
            let npOption = hashMap[i]

            if npOption.IsSome then
                let np = npOption.Value

                if not np.isDeleted then
                    foldRec (i + 1) (f res np.key np.value)
                else
                    foldRec (i + 1) res
            else
                foldRec (i + 1) res
        else
            res

    foldRec 0 init


let backFold (f: 'T -> 'K -> 'V -> 'T) (init: 'T) (hashMap: HashMap<'K, 'V>) : 'T =
    let size = hashMap.Length

    let rec foldRec i res =
        if i >= 0 then
            let npOption = hashMap[i]

            if npOption.IsSome then
                let np = npOption.Value

                if not np.isDeleted then
                    foldRec (i - 1) (f res np.key np.value)
                else
                    foldRec (i - 1) res
            else
                foldRec (i - 1) res
        else
            res

    foldRec (size - 1) init

let merge (hashMap1: HashMap<'K, 'V>) (hashMap2: HashMap<'K, 'V>) : HashMap<'K, 'V> =
    if (getSize hashMap1 = 0) then
        hashMap2
    else if (getSize hashMap2 = 0) then
        hashMap1
    else
        let initSize = max (getSize hashMap1) (getSize hashMap2)
        let sumHm = create initSize

        let rec iterOverHM i (hm: HashMap<'K, 'V>) (resHm: HashMap<'K, 'V>) =
            if i >= 0 then
                let ndOption = hm[i]

                if ndOption.IsSome then
                    let nd = ndOption.Value

                    if not nd.isDeleted then
                        add (nd.key, nd.value) resHm |> iterOverHM (i - 1) hm
                    else
                        iterOverHM (i - 1) hm resHm
                else
                    iterOverHM (i - 1) hm resHm
            else
                resHm

        sumHm
        |> iterOverHM (getSize hashMap1 - 1) hashMap1
        |> iterOverHM (getSize hashMap2 - 1) hashMap2

let compare (hashMap1: HashMap<'K, 'V>) (hashMap2: HashMap<'K, 'V>) : bool =
    if hashMap1.Length <> hashMap2.Length then
        false
    else
        let rec recCompare i res =
            if res then
                if i < hashMap1.Length then
                    let ndOption = hashMap1[i]

                    if (ndOption.IsSome && not ndOption.Value.isDeleted) then
                        let nd = ndOption.Value
                        let ndOption2 = get nd.key hashMap2
                        let cmp = ndOption2.IsSome && ndOption2.Value = nd.value
                        recCompare (i + 1) cmp
                    else
                        recCompare (i + 1) res
                else
                    res
            else
                res

        recCompare 0 true
