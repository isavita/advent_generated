using MD5

salt = read("input.txt", String)
global index = 0
global keys = 0

while keys < 64
    hash = bytes2hex(md5(string(salt, index)))
    if occursin(r"(.)\1\1", hash)
        char = match(r"(.)\1\1", hash)[1]
        for i in 1:1000
            next_hash = bytes2hex(md5(string(salt, index + i)))
            if occursin(string(char, char, char, char, char), next_hash)
                global keys += 1
                println(index)
                break
            end
        end
    end
    global index += 1
end