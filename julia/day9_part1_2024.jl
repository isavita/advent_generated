
function solve()
    line = strip(read("input.txt", String))
    disk = Char[]
    fileID = 0
    isFile = true
    for c in line
        length = parse(Int, string(c))
        if isFile
            append!(disk, fill(Char('0' + fileID), length))
            fileID += 1
        else
            append!(disk, fill('.', length))
        end
        isFile = !isFile
    end

    while true
        lfree = findfirst(==('.'), disk)
        if lfree === nothing
            break
        end
        rfile = findlast(!=('.'), disk)
        if rfile === nothing || rfile <= lfree
            break
        end
        disk[lfree] = disk[rfile]
        disk[rfile] = '.'
    end

    checksum = 0
    for (i, b) in enumerate(disk)
        if b != '.'
            id = Int(b - '0')
            checksum += (i - 1) * id
        end
    end
    println(checksum)
end

solve()
