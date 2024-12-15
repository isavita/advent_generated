
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

    files = Tuple{Int, Int, Int}[]
    curID = -1
    start = 0
    for (i, b) in enumerate(disk)
        if b == '.'
            curID = -1
            continue
        end
        id = Int(b - '0')
        if id != curID
            curID = id
            start = i
        end
        if i == length(disk) || (i + 1 <= length(disk) && disk[i+1] - '0' != id)
            push!(files, (id, start, i))
        end
    end

    for i in length(files):-1:1
        f = files[i]
        fileLen = f[3] - f[2] + 1
        leftmostSpan = -1
        spanLen = 0
        for j in 1:f[2]-1
            if disk[j] == '.'
                if spanLen == 0
                    leftmostSpan = j
                end
                spanLen += 1
                if spanLen == fileLen
                    break
                end
            else
                spanLen = 0
                leftmostSpan = -1
            end
        end
        if leftmostSpan != -1 && spanLen == fileLen
            for x in f[2]:f[3]
                disk[x] = '.'
            end
            for x in 0:fileLen-1
                disk[leftmostSpan+x] = Char('0' + f[1])
            end
        end
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
