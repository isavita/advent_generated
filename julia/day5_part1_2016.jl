using MD5

function findPassword(doorID)
    password = ""
    i = 0
    while length(password) < 8
        hash = bytes2hex(md5(doorID * string(i)))
        if startswith(hash, "00000")
            password *= hash[6]
        end
        i += 1
    end
    return password
end

doorID = strip(read("input.txt", String))
password = findPassword(doorID)
println(password)