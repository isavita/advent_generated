using Pkg
Pkg.add("MD5")
using MD5

# Read the input from the file "input.txt"
secret_key = read("input.txt", String)

# Function to find the lowest positive number that produces a hash starting with at least 5 zeroes
function find_lowest_number(key)
    i = 1
    while true
        hash_str = string(key, i)
        hash_value = bytes2hex(md5(hash_str))
        if startswith(hash_value, "00000")
            return i
        end
        i += 1
    end
end

# Print the answer
println(find_lowest_number(secret_key))