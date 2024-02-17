
function firstNUnique(s::String, n::Int)
    for i in n:length(s)
        b = collect(s[i-n+1:i])
        if length(b) == length(Set(b))
            return i
        end
    end
    return -1
end

function readAll(path::String)
    file = read(path, String)
    return strip(file)
end

function SetOf(b::Vector{UInt8})
    m = Set{UInt8}()
    for c in b
        push!(m, c)
    end
    return collect(m)
end

s = readAll("input.txt")
s = String(s)  # Convert SubString to String
println(firstNUnique(s, 4))
