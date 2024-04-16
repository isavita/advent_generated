using MD5

const hashCache = Dict{String,String}()

function getStretchedMD5Hash(input::String)
    if haskey(hashCache, input)
        return hashCache[input]
    end
    hash = bytes2hex(md5(input))
    for i in 1:2016
        hash = bytes2hex(md5(hash))
    end
    hashCache[input] = hash
    return hash
end

function findTriplet(hash::String)
    for i in 1:length(hash)-2
        if hash[i] == hash[i+1] == hash[i+2]
            return hash[i:i]
        end
    end
    return ""
end

function main()
    salt = strip(read("input.txt", String))
    keys = 0
    index = 0
    while keys < 64
        index += 1
        hash = getStretchedMD5Hash(salt * string(index))
        triplet = findTriplet(hash)
        if !isempty(triplet)
            for i in 1:1000
                nextHash = getStretchedMD5Hash(salt * string(index+i))
                if occursin(repeat(triplet, 5), nextHash)
                    keys += 1
                    break
                end
            end
        end
    end
    println(index)
end

main()