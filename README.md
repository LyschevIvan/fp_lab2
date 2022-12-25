# Функциональное программирование
## Лабораторная работа 2
### Вариант HashMap (OpenAdressing)
**Выполнил:** Лыщев Иван Антонович \
**Группа:** P34112 \
**Преподаватель:** Пенской Александр Владимирович

### Требования к стуктуре
>Функции:
>- добавление и удаление элементов;
>- фильтрация;
>- отображение (map);
>- свертки (левая и правая);
>- структура должна быть моноидом.

> Требования к библиотеке
>- Структуры данных должны быть неизменяемыми.\
>- Библиотека должна быть протестирована в рамках unit testing.\
>- Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства монойда).\
>- Структура должна быть полиморфной.\
>- Требуется использовать идиоматичный для технологии стиль программирования.

>Разрешение коллизий Open Address \
> При совпадении результатов hash-функции выбирается другая пустая ячейка\
> В моей реализации выбирается следующая ячейка

### Реализация
Сама структура является массивом узлов. Каждый узел хранит в себе пару ключ-значение и признак удаления.\
При этом в F# для реализации записи пустых элементов требуется оборачивать узел в структуру Option. 
```f#
type node<'K, 'V> =
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
    
type hashMap<'K, 'V> = Option<node<'K, 'V>>[]
```
Для создания HashMap создается массив длины n из элементов Option.None т.е. пустых 
```f#
let create<'K, 'V> (count: int): hashMap<'K, 'V> = Array.create count Option.None
```
При добавлении элементов исходный массив дублируется для неизменяемости исходного
```f#
let add (key: 'K, v : 'V) (hashTable: hashMap<'K, 'V>) =
    let innerMap = Array.copy hashTable
    addRec (getIndex (hash key) innerMap) (key, v) innerMap
```
Функция addRec итеративно ищет подходящую ячейку. Она должла быть либо пустой, либо с флагом isDeleted = true, либо с совпадающим ключем.
```f#
let  rec private addRec (i : int) (key: 'K, v: 'V) (hashTable: hashMap<'K, 'V>) =
    let size = hashTable.Length
    if (i < size) then
        let ndOption = hashTable[i]
        if ndOption.IsNone then
            hashTable[i] <- Option.Some (node (key, v))
            hashTable
        else
            let nd = ndOption.Value
            if nd.isDeleted then
                hashTable[i] <- Option.Some (node (key, v))
                hashTable
            else if (nd.key <> key) then
                addRec  (getNext i) (key, v) hashTable
            else
                hashTable[i] <- Option.Some (node (key, v))
                hashTable
    else
        let newHM = resize hashTable
        addRec (getIndex (hash key) newHM) (key, v) newHM
and resize (hashMap: hashMap<'K, 'V>) : hashMap<'K, 'V> = ...
```
Когда индекс массива выходит за границы размера, то создается новый массив с размером в два раза больше. Все элементы переносятся в него и добавляется требуемый 
```f#
and resize (hashMap: hashMap<'K, 'V>) : hashMap<'K, 'V> =
    let size = getSize hashMap
    let newHashMap = create<'K, 'V> (size * 2)

    let rec transfer i (hMap: hashMap<'K, 'V>) : hashMap<'K, 'V> =
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
```
Удаление осуществляется поиском нужного индекса по ключу и узел либо помечается удаленным, либо isDeleted
```f#
let delete (key:'K) (hashMap: hashMap<'K, 'V>) =
    let hash = hash key
    let index = findIndexByKey (getIndex hash hashMap) key hashMap
    if index >= 0 then
        markAsDeleted index hashMap
    else 
        hashMap
```
Функция поиска индекса возвращает -1, если элемент не найден
```f#
let rec private findIndexByKey i (key : 'K) (hashMap: hashMap<'K, 'V>) =
    let size = getSize hashMap
    if i < size then
        let nd = hashMap[i]
        if nd.IsNone then
            -1
        else if nd.Value.isDeleted then
            if(nd.Value.key = key) then -1
            else findIndexByKey (getNext i) key hashMap
        else if key = nd.Value.key then
            i
        else
            findIndexByKey (getNext i) key hashMap
    else -1
```
В зависимости от того, последний у нас элемент или нет, он помечается Deleted или присваивается None
```f#
let private markAsDeleted i (hashMap: hashMap<'K, 'V>) : hashMap<'K, 'V>=
    let innerMap = Array.copy hashMap
    let size = hashMap.Length
    let ndOption = innerMap[i]
    if(ndOption.IsSome) then
        let nd = ndOption.Value
        if i = (size-1) then
            innerMap[i] <- Option.None
        else
            innerMap[i] <- Option.Some (node<'K, 'V>(nd.key, nd.value, true))
    innerMap
```
Функция фильтрации проходится по всем элементам и если они не None и функция возвращает false, то узел помечается deleted
```f#
let filter (f: node<'K, 'V> -> bool) (hashMap: hashMap<'K, 'V>) =
    let innerMap = Array.copy hashMap
    let size = hashMap.Length

    let rec doFilter i (hm: hashMap<'K, 'V>) : hashMap<'K, 'V> =
        if (i < size) then
            let ndOption = hm[i]

            if (ndOption.IsSome) then
                if not (f (hm[i].Value)) then
                    markAsDeleted i hm |> doFilter (i + 1)
                else
                    doFilter (i + 1) hm
            else
                doFilter (i + 1) hm
        else
            hm

    doFilter 0 innerMap
```
Функция map проходится по всем элементам и если они не None, то узел добаляется к результирующему массиву со значение функции

```f#
let map (f: 'K -> 'V -> 'U) (hashMap: hashMap<'K, 'V>) =
    let size = hashMap.Length
    let innerMap = create<'K, 'U> size

    let rec mapRec i =
        if i < size then
            let ndOption = hashMap[i]

            if ndOption.IsSome then
                let nd = ndOption.Value

                if (not nd.isDeleted) then
                    innerMap[i] <- Option.Some(node (nd.key, f nd.key nd.value, nd.isDeleted))

                mapRec (i + 1)
            else
                mapRec (i + 1)
        else
            innerMap

    mapRec 0
```
Функция Fold проходится по всем элементам, если они не None или Deleted, то высчитывается state с помощью передаваемой функции и переходится к следующему элементу.
```f#
let fold (f: 'T -> 'K -> 'V -> 'T) (init: 'T) (hashMap: hashMap<'K, 'V>) : 'T =
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
```
Функция backFold делает тоже самое, но начиная с конца
```f#
let backFold (f: 'T -> 'K -> 'V -> 'T) (init: 'T) (hashMap: hashMap<'K, 'V>) : 'T =
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
```
Функция Merge служит для слияния двух HashMap. Создается массив с размером наибольшей передаваемой HashMap и туда сначала добавляются элементы из первого HashMap, потом из другой 
```f#
let merge (hashMap1: hashMap<'K, 'V>) (hashMap2: hashMap<'K, 'V>) : hashMap<'K, 'V> =
    if (getSize hashMap1 = 0) then
        hashMap2
    else if (getSize hashMap2 = 0) then
        hashMap1
    else
        let initSize = max (getSize hashMap1) (getSize hashMap2)
        let sumHm = create initSize

        let rec iterOverHM i (hm: hashMap<'K, 'V>) (resHm: hashMap<'K, 'V>) =
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
```
### Тесты
В каждом тесте, кроме PBT, используются начальные значения, которые создаются функцией initHm
```f#
let initHm =
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
```
**Проверка увеличения размера массива.** Когда мы добавляем элемент с ключем size, он помещается в конец. Также с ключем -1.
Соответственно когда добавляется с ключем -1, он упирается в размер массива и требуется увеличение. 
```f#
[<Theory>]
[<InlineData(4)>]
[<InlineData(40)>]
[<InlineData(12)>]
[<InlineData(7)>]
let ``test resize`` n =
    let hm = create n
    let init_size = hm |> getSize
    let size = hm |> add (init_size - 1,6)  |> getSize
    let size2 = hm |> add (init_size - 1,6) |> add (-1,10) |> getSize
    Assert.Equal(size * 2, size2)
```
**Проверка удаления.** Элемент удаляется и потом проверяется, возвращает ли поиск индекса -1
```f#
[<Theory>]
[<InlineData(0)>]
[<InlineData(1)>]
[<InlineData(-1)>]
[<InlineData(5)>]
let ``test delete`` key =
    let hm = initHm |> delete key
    Assert.False(hasKey key hm)
```
**Проверка фильтра.** Отсеиваются все значения с value. Потом проверятся, существует ли такой элемент
```f#
[<Theory>]
[<InlineData(1, -1)>]
[<InlineData(0, 3)>]
[<InlineData(-1, 4)>]
[<InlineData(5, 5)>]
let ``test filter`` key value =
    let hm = initHm |> filter (fun n -> (n.value <> value))
    Assert.False(hasKey key hm)
```
**Проверка свойства map.** Применяется функция приводящая value к string и проверяется тип резуьтирующей HashMap 
```f#
[<Property>]
let ``test map property`` (data: (int * int) list) =
    let hm = init 4 data |> map (fun k v -> $"{k} {v}") |> box
    hm :? hashMap<int, string>
```
**Проверка fold.** Проверяются результаты некоторых функций для тестовой HashMap
```f#
[<Fact>]
let ``test fold`` () =
    let hm = initHm 
    Assert.Equal(15, fold (fun state k v -> state + v) 0 hm)
    Assert.Equal(-15, fold (fun state k v -> state - v) 0 hm)
    Assert.Equal(-240, fold (fun state k v -> state * v) 1 hm)
```
**Проверка backFold.** Аналогично fold.
```f#
[<Fact>]
let ``test fold`` () =
    let hm = initHm 
    Assert.Equal(15, fold (fun state k v -> state + v) 0 hm)
    Assert.Equal(-15, fold (fun state k v -> state - v) 0 hm)
    Assert.Equal(-240, fold (fun state k v -> state * v) 1 hm)
```
**Проверка свойства add.** Проверяется, добавилось ли значение и правильное ли оно при получении.
```f#
[<Property>]
let ``test property add`` (node: (int * int), data: (int * int) list) =
    let hm = init 2 data
    let (k, v) = node
    let addedValue = hm |> add node |> get k |> Option.get
    addedValue = v
```
**Проверка нейтрального элемента.** Нейтральным элементом является пустая HashMap. Соответственно после слияния с другой HashMap сравнение должно вернуть true 
```f#
[<Property>]
let ``test property neutral element`` (data: (int * int) list) =
    let hm = init 4 data
    let neutral = create 1
    let sumHmHash = merge hm neutral
    compare sumHmHash hm
```
**Проверка ассоциативности** 
```f#
[<Property>]
let ``test associativity`` (data1: (int * int) list, data2: (int * int) list, data3: (int * int) list) =
    let hm1 = init 2 data1
    let hm2 = init 2 data2
    let hm3 = init 2 data3
    let merge12 = merge hm1 hm2
    let merge23 = merge hm2 hm3
    compare (merge merge12 hm3) (merge hm1 merge23))
```
### Выводы
В ходе выполнения лабораторной работы я столкнулся с парой проблем. 
Первая связана с невозможностью присвоения null воощбе ни к чему. Потом я пытался использовать структуру Nullable, но оказалось, что она работает только как "обертка" классов.
Только потом я узнал об Option. ~~Также столкнулся с тем, что 2 функции addRec и resize переиспользуют логику друг друга и чтобы это реализовать пришлось "прокидывать" функцию addRec в resize. 
Мне это кажется плохим решением, но других способов я не нашел.~~\
**Update:** Eсли функция рекурсивная, то объявить одну внутреннюю функцию можно после с помощью
конструкции "*let rec ... and ...*"
