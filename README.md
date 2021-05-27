# netbuff

Netbuff implements simple and efficient data serialization and de-serialization. Any data structure can be encoded and decoded. Pointers and collections are dereferenced and copied into bite form from witch they can be reallocated on the other side. Data-transfer is also safe, buffer inserts hashed type before each encoded value. This does not hurt performance as hashing is performed at compile time. This not only improves safety but also gives space for nice functionality.

## Decode matching

Netbuff offers little dls that makes dealing with optional data little bit nicer. Here is the unit test testing the capability:

```nim
test "dls":
    var buff = initBuffer()

    buff.write(10.int32)
    buff.write("hello")
    buff.write(true)
    buff.write(10.4)

    var reader = buff.reader

    var
        a: int32
        b: string
        c: bool

    while true:
        decodeCase reader, res:
            @int32: a = res
            @string: b = res
            @bool: c = res
            @any: break

    check a == 10
    check b == "hello"
    check c == true
    check reader.read(float) == 10.4
```

`decodeCase` takes buffer to read and name of a variable that will be injected to each case branch. Branch with `any` as type acts as else branch and must be last branch.

## transferring primitives

As you may noticed you can use one proc to write anything except tables (i will not support reflection for tables). Reading can be also performed by same method. Methods for each type are generated by macro and wrapped inside generic method. Ths way macro is called only once per each type. 

```nim
test "primitives":
    var buff = initBuffer()

    buff.write(10.int32)
    buff.write(true)
    buff.write("hello")
    buff.write(10.2f32)
    buff.write(PI)

    try:
        discard buff.read(int)
        check false
    except AccessViolationDefect:
        discard

    var reader = buff.reader

    check reader.read(int32) == 10.int32
    
    try:
        discard reader.read(int32)
        check false
    except ValueError:
        discard
    
    check reader.read(bool) == true
    check reader.read(string) == "hello"
    check reader.read(float32) == 10.2f32
    check reader.read(float64) == PI
```

## transferring object 

Lot more effective way of encoding and decoding is encapsulating data int object and sending them instead. If object does not contain any pointer or seq quick moveMem is used. On the other hand if you supply a reference tree it will be dereferenced anc written completely. When you decode all pointer will be reallocated and tree rebuild. Following test demonstrates such decoding.

```nim
test "complex":
    type 
        Vector = tuple
            x, y: float
        StackAllocated = object
            pos, vel, aim: Vector
            health: int

    var buff = initBuffer()

    let obj = StackAllocated(
        pos: (100.3, 300.4),
        vel: (20.3, 10.2),
        aim: (20.2, 10.2),
        health: 30
    )

    buff.write(obj)

    var list: seq[seq[ref int32]]

    list.setLen(10)

    for i, r in list.mpairs:
        r.setLen(10)
        for j, e in r.mpairs:
            if i mod 2 == j mod 2:
                new e
                e[] = i.int32 + j.int32

    buff.write(list)

    var re = buff.reader

    check re.read(StackAllocated) == obj

    let decoded = re.read(seq[seq[ref int32]])

    for y in 0..<10:
        for x in 0..<10:
            check decoded[y][x] == list[y][x] or decoded[y][x][] == list[y][x][]
```

## bugs

Runtime bugs should not happen though compile time especially regarding the hashing can. Some structures mights not be supported. Feel free to open the issue an it will be fixed in reasonable time (if possible to fix).

While developing this package i encountered a strange bug with nested tuples. I suspect it is somewhere in compiler and whatever i tried i could not fix it. Types like `(int, (int, (int, ref int)))` are thus not supported and will cause compile time error.