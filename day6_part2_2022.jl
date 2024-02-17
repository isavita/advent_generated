
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

# Read the file content as a String
content = readAll("input.txt")

# Convert the content to a regular String to avoid the SubString error
content = string(content)

println(firstNUnique(content, 14))
