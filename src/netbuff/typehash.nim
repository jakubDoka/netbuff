import macros
from math import ceil

proc uniqueString(n: NimNode, str: var string) =
    # almost random algorithm that tries to make as unique string as possible based of impl
    # ({<|[ its horrible ]|>})
    case n.kind:
        of nnkBracketExpr:
            case n[0].strVal:
            of "typeDesc":
                uniqueString(n[1], str)
            else:
                uniqueString(n[0], str)
        of nnkSym:
            let impl = n.getImpl
            if impl.kind != nnkNilLit:
                str.add(impl.treeRepr)
            str.add(n.getType.repr)
            uniqueString(impl, str)
        of nnkTypeDef:
            case n[2].kind:
            of nnkObjectTy:
                for id in n[2][2]:
                    uniqueString(id[^2], str)
            of nnkTupleTy:
                for id in n[2]:
                    uniqueString(id[^2], str)
            of nnkDistinctTy:
                uniqueString(n[2][0], str)
            else:
                discard
        else:
            discard


macro hashMacro(t: type): uint32 =
    var r: string
    uniqueString(t.getType, r)

    r.setLen(ceil(r.len / 4).int * 4)
    var res: uint64
    for i in countup(0, r.len-1, 4):
        for j in 0..<4:
            res += r[i+j].uint64.shl(8 * j)

    return newLit(uint32(res mod uint32.high.uint64))

func hash*[T](): uint32 =
    ## hash creates almost unique hash of any given type
    ## chances are really low to get a match but it can
    ## happen
    hashMacro(T)

when isMainModule:
    type All = object
        x, y, v: int32
        r: float
    echo hash[float]()
    echo hash[int]()
    echo hash[Slice]()
    echo hash[All]()
    echo hash[float]()
    assert hash[seq[seq[int]]]() != hash[seq[seq[float]]]()
