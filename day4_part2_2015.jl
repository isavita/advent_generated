using MD5

secret_key = strip(read("input.txt", String))
number = 0

while true
    global number
    hash = bytes2hex(md5(secret_key * string(number)))
    if startswith(hash, "000000")
        println(number)
        break
    end
    number += 1
end