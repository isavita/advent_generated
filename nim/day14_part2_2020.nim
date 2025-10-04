
import std/strutils
import std/tables
import std/sequtils

proc generateAddresses(mask: string, address: int64): seq[int64] =
    var
        baseAddress = address
        floating: seq[int] = @[]

    for i in 0..35:
        let bitPos = 35 - i
        case mask[i]:
        of '1':
            baseAddress = baseAddress or (1'i64 shl bitPos)
        of 'X':
            floating.add(bitPos)
        else:
            discard

    result = newSeq[int64](1 shl floating.len)

    for i in 0..<result.len:
        var modAddress = baseAddress
        
        for j, bitPos in floating:
            if (i and (1 shl j)) != 0:
                modAddress = modAddress or (1'i64 shl bitPos)
            else:
                modAddress = modAddress and not (1'i64 shl bitPos)
        
        result[i] = modAddress

proc main =
    var
        mask: string = ""
        mem: Table[int64, int64]
        
    for line in lines("input.txt"):
        let trimmedLine = line.strip()
        if trimmedLine.startsWith("mask = "):
            mask = trimmedLine[7..^1]
        elif trimmedLine.startsWith("mem"):
            let eqPos = trimmedLine.find('=')
            let bracketStart = trimmedLine.find('[')
            let bracketEnd = trimmedLine.find(']')
            
            if eqPos > 0 and bracketStart > 0 and bracketEnd > bracketStart:
                let address = trimmedLine[bracketStart+1 .. bracketEnd-1].parseInt.int64
                let value = trimmedLine[eqPos+1 .. ^1].strip.parseInt.int64
                
                for addr in generateAddresses(mask, address):
                    mem[addr] = value

    var totalSum: int64 = 0
    for value in mem.values:
        totalSum += value
        
    echo totalSum

when isMainModule:
    main()
