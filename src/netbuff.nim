
import macros, netbuff/typehash

template debug(ifDebug, ifRelease: untyped): untyped =
    when defined(debug):
        ifDebug
    else:
        ifRelease

type
    Buffer* = object
        ## Object holding information about encoding
        ## or decoding process. It is designed for static
        ## data transfer focused on performance and memory
        ## efficiency. Its methods heavy utilize compile time
        ## reflection and also manages to optimize some encoding
        ## scenarios
        data*: ref seq[byte]
        view: Slice[int]
        readable: bool
        cursor: int

proc initBuffer*(data = newSeq[byte](0), writing = false): Buffer =
    ## Creates new buffer from data. This operation reallocates it for safety
    ## though slicing into buffer will not.
    new result.data
    result.data[] = data
    result.readable = data.len != 0 and not writing

template current*(b: Buffer): ptr byte = b.data[b.cursor].addr

template check(buf: Buffer, sz: int) =
    block:
        let total = buf.cursor+sz
        if buf.view.b < total:
            raise newException(OverflowDefect, "buffer overflowed (" &
                    $buf.view.b & " < " & $total & ")")

proc wrap(n: NimNode): NimNode = newTree(nnkBracketExpr, ident("typeDesc"), n)

template unrecognized(n: NimNode, ident: string) =
    error("unrecognized " & ident & "(unsupported)[" & n.lineInfo & "]")

macro readComplex(b: var Buffer, t: type): untyped =
    var calls = newNimNode(nnkStmtList)

    var tp = t.getType[1]
    if tp.kind == nnkSym:
        tp = tp.getType

    case tp.kind:
    of nnkSym:
        if tp.strVal != "string": error("sym that refers to heap that is not string")
        calls.add(quote do: result = `b`.readString())
    of nnkObjectTy:
        for r in tp[2]:
            var tp = r.getType
            tp = wrap(tp)
            calls.add(quote do: result.`r` = `b`.readU(`tp`))
    of nnkBracketExpr:
        let t = wrap(tp[1])
        case tp[0].strVal:
            of "ref":
                calls.add(quote do:
                    if `b`.readU(bool):
                        new result
                        result[] = `b`.readU(`t`)
                )
            of "ptr":
                calls.add(quote do:
                    if `b`.readU(bool):
                        result = cast[ptr `t`](alloc(sizeof(`t`)))
                        result[] = `b`.readU(`t`)
                )
            of "seq":
                calls.add(quote do: result = `b`.readSeq(`t`))
            else:
                unrecognized(tp, "bracket expression")
    else:
        unrecognized(tp, "typeDesc")

    return calls

macro writeComplex(b: var Buffer, v: typed): untyped =
    var calls = newNimNode(nnkStmtList)

    let tp = v.getType

    case tp.kind:
    of nnkSym:
        if tp.strVal != "string": error("sym that refers to heap that is not string")
        calls.add(quote do: `b`.writeString(`v`))
    of nnkObjectTy:
        for r in tp[2]:
            let tp = r.getType
            calls.add(quote do: `b`.writeU(`v`.`r`))
    of nnkBracketExpr:
        case tp[0].strVal:
            of "ref", "ptr":
                calls.add(quote do:
                    if `v` == nil:
                        `b`.writeU(false)
                    else:
                        `b`.writeU(true)
                        `b`.writeU(`v`[])
                )
            of "seq":
                calls.add(quote do:
                    `b`.writeSeq(`v`)
                )
            else:
                unrecognized(tp, "bracket expression")
    else:
        unrecognized(tp, "typeDesc")

    return calls



proc isSimple(tp: NimNode): bool =
    let t =
        case tp.kind:
            of nnkBracketExpr: tp
            else: tp.getType

    assert t[0].strVal == "typeDesc"

    let tp = t[1]

    case tp.kind:
    of nnkSym:
        case tp.strVal:
        of "pointer": error("pointer is not supported")
        of "string": return false
        let tp = tp.getType
        if t.kind == nnkSym: return true
        case tp.kind:
        of nnkObjectTy:
            for id in tp[2]:
                if not isSimple(wrap(id.getType)):
                    return false
        else:
            return true
    of nnkBracketExpr:
        case tp[0].strVal:
        of "ptr", "ref", "seq": return false
        of "tuple":
            for i in 1..<tp.len:
                if not isSimple(wrap(tp[i])):
                    return false
        else: return true
    else:
        return true
    return true

macro assertSimpleMacro(tp: type): untyped = return newLit(isSimple(tp))

func isSimple*[T](): bool =
    ## returns whether structure does not contain any pointers
    assertSimpleMacro T

func writeU*[T](b: var Buffer, value: T) {.inline.}
func readU*(b: var Buffer, T: type): T {.inline.}

func readSeq*(b: var Buffer, tp: type): seq[tp] {.inline.} =
    ## reads sequence from buffer. This is N(1) if
    ## seq does not contain pointers.
    var l = b.readU(int32)
    result.setLen(l.int)
    when isSimple[tp]():
        let size = l*sizeof(tp)
        moveMem(result[0].addr, b.current, size)
        b.cursor.inc(size)
    else:
        for i in 0..<l:
            result[i] = b.readU(tp)

func writeSeq*[T](b: var Buffer, s: seq[T]) {.inline.} =
    ## Writes sequence to buffer. This is N(1) if
    ## seq does not contain pointers.
    let l = s.len
    b.writeU(l.int32)
    if l == 0: return
    let size = l*sizeof(T)
    when isSimple[T]():
        let o = b.data[].len
        b.data[].setLen(o+size)
        moveMem(b.data[o].addr, s[0].unsafeAddr, size)
    else:
        for e in s:
            b.writeU(e)

template readString*(b: var Buffer): string =
    ## Reads string from buffer
    cast[string](b.readSeq(byte))

template writeString*(b: var Buffer, str: string) =
    ## Writes string to buffer.
    b.writeSeq(cast[seq[byte]](str))

func readU*(b: var Buffer, T: type): T =
    if not b.readable:
        raise newException(AccessViolationDefect, "buffer is not for reading, use reader method")

    when isSimple[T]():
        const sz = sizeof(T)

        b.check(sz)

        moveMem(result.addr, b.current, sz)

        b.cursor.inc(sz)
    else:
        readComplex(b, T)

template read*(b: var Buffer, T: untyped): untyped =
    ## reads any structure from buffer. All pointers are allocated if needed
    ## and everything is reconstructed. Serializing stack allocated structures
    ## is especially fast. Structure is recursively checked for pointers to
    ## justify the optimization.
    if b.readU(uint32) != hash[T]():
        b.cursor.dec(sizeof(uint32))
        raise newException(ValueError, "type hash does not match")

    b.readU(T)

template assertReadable(b: Buffer) =
    if b.readable:
        raise newException(AccessViolationDefect, "buffer is not for writing")

func writeU*[T](b: var Buffer, value: T) =
    assertReadable b

    when isSimple[T]():
        const sz = sizeof(T)
        let o = b.data[].len

        b.data[].setLen(o+sz)

        moveMem(b.data[o].addr, value.unsafeAddr, sz)
    else:
        writeComplex(b, value)

template write*[T](b: var Buffer, value: T) =
    ## Writes any structure into buffer, pointers are dereferenced
    ## and serialized accordingly, collections are also supported.
    ## Nil pointer takes only one byte. Each write also writes the
    ## type hash, hash is calculated at compile time though encoding
    ## objects is still better the encoding each value separately.
    ## When you are writing a packet of related data, its always better
    ## to put data to object and let macro generate boilerplate.
    b.writeU(hash[T]())
    b.writeU(value)


func reader*(b: Buffer): Buffer {.inline.} =
    ## Creates reader from current instance. It's mostly useful for testing.
    result.data = b.data
    result.readable = true
    result.view.b = b.data[].len

func slice*(b: var Buffer, size: int): Buffer {.inline.} =
    ## Takes a slice of current buffer. No allocations.
    ## View of new buffer will start at current buffer cursor
    ## and end ta cursor + size. Reading past the slice is error
    ## even if there is accessible data.
    assertReadable b
    b.check(size)

    result.data = b.data
    result.view.a = b.cursor
    result.view.b = b.cursor + size
    result.cursor = b.cursor
    result.readable = true

template clear*(b: var Buffer) =
    # clears the buffer for reuse
    if b.view.a != 0:
        raise newException(AccessViolationError, "only original buffer can be cleared")
    b.readable = false
    b.data[].setLen(0)

macro decodeCase*(buff: var Buffer, variable, body: untyped): untyped =
    ## little dls that simplifies matching of data. All macro does is replacing
    ## simplified body with case statement. Here is little demonstration of syntax.
    ## 
    ##  .. block:: nim
    ##  decodeCase buff, variable:
    ##      @float:
    ##          echo "we found a float"
    ##      @int:
    ##          echo "ve found a int equal to " & $variable
    ##      @seq[int]:
    ##          for i in variable:
    ##              echo i
    ##      @any:
    ##          echo "we found something that is not listed above" 
    var caseStmt = newTree(nnkCaseStmt, quote do: `buff`.readU(uint32))

    for b in body:
        case b.kind:
        of nnkPrefix:
            case b[1].strVal:
            of "any":
                var st = b[2]
                st.insert(0, quote do:
                    `buff`.cursor.dec(sizeof(uint32))    
                )
                caseStmt.add(newTree(nnkElse, st))
            else:
                let tp = b[1]
                var st = b[2]
                st.insert(0, quote do:
                    var `variable` = `buff`.readU(`tp`)
                )
                caseStmt.add(newTree(nnkOfBranch, quote do: hash[`tp`](), st))
        else:
            error("invalid syntax" & b.lineInfo)

    return caseStmt

when isMainModule:
    type
        Vec = object
            x, y: float
        Mec = object
            x, y: float
            l: ref float
        F = distinct int

    var buff = initBuffer()

    buff.write(1.F)
    buff.write("hello")
    buff.write(Vec(x: 10, y: 20))
    buff.write(10.int32)
    buff.write(100.2)

    var f: ref float; new f; f[] = 20.5
    buff.write(Mec(x: 10, y: 30, l: f))
    buff.write(@[1, 3, 4])

    var complex = newSeq[ref Vec](3)
    for e in complex.mitems:
        new e
        e[] = Vec(x: 10, y: 30)
    buff.write(complex)

    var re = buff.reader

    assert re.read(F).int == 1
    assert re.read(string) == "hello"
    assert re.read(Vec) == Vec(x: 10, y: 20)
    assert re.read(int32) == 10
    assert re.read(float) == 100.2

    decodeCase re, m:
        @Mec:
            assert m.x == 10
            assert m.y == 30
            assert m.l[] == 20.5
        @Vec:
            assert false
        @any:
            assert false

    assert re.read(seq[int]) == @[1, 3, 4]
    for e in re.read(seq[ref Vec]):
        assert e[] == Vec(x: 10, y: 30)
