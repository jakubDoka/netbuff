
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

proc wrap(n: NimNode, tpd: NimNode): NimNode =
  newTree(nnkBracketExpr, tpd, n)

template unrecognized(n: NimNode, ident: string) =
  error("unrecognized " & ident & "(unsupported)[" & n.lineInfo & "]")

macro readComplex(b: var Buffer, t: type): untyped =
  var calls = newNimNode(nnkStmtList)

  let tpd = t.getType[0]
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
      tp = wrap(tp, tpd)
      calls.add(quote do: result.`r` = `b`.readU(`tp`))
  of nnkBracketExpr:
    let t = wrap(tp[1], tpd)
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
      of "tuple":
        for i in 0..<tp.len-1:
          let tp = wrap(tp[i+1], tpd)
          calls.add(quote do:
            result[`i`] = `b`.readU(`tp`)
          )
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
      of "tuple":
        for i in 0..<tp.len-1:
          calls.add(quote do:
            `b`.writeU(`v`[`i`])
          )
      else:
        unrecognized(v, "bracket expression")
  else:
    unrecognized(v, "typeDesc")

  return calls




proc isSimple(tp: NimNode): bool =
  template err = error("check does not account for this object, please open the issue")

  case tp.kind:
  of nnkSym:
    case tp.strVal:
    of "pointer": error("pointer is not supported")
    of "string": return false
    else: discard

    let tp = tp.getType
    case tp.kind:
    of nnkObjectTy:
      for id in tp[2]:
        if not isSimple(id.getType):
          return false
      return true
    of nnkSym:
      return true
    of nnkBracketExpr:
      case tp[0].strVal:
      of "distinct":
        return isSimple(tp[1])
      else:
        err()
    else:
      err()
  of nnkBracketExpr:
    case tp[0].strVal:
    of "ptr", "ref", "seq": return false
    of "tuple":
      for i in 1..<tp.len:
        if not isSimple(tp[i]):
          return false
      return true
    else: err()
  else:
    err()
  err()



macro assertSimpleMacro(tp: type): untyped = return newLit(isSimple(tp.getType[1]))

func isSimple[T](): bool =
  ## returns whether structure does not contain any pointers
  assertSimpleMacro T

template simple*(tp: type): bool = isSimple[tp]()

func writeU*[T](b: var Buffer, value: T) {.inline.}
func readU*(b: var Buffer, T: type): T {.inline.}

func readSeq*(b: var Buffer, tp: type): seq[tp] {.inline.} =
  ## reads sequence from buffer. This is N(1) if
  ## seq does not contain pointers.
  var l = b.readU(int32)
  result.setLen(l.int)
  when simple tp:
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
  when simple T:
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

  when simple T:
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

  when simple T:
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
