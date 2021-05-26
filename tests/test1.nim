# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest, math

import netbuff

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