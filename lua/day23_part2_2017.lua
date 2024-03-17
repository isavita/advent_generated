function is_prime(n)
    for i=2,math.floor(math.sqrt(n)) do
        if n%i==0 then
            return false
        end
    end
    return true
end

local b=57*100+100000
local c=b+17000
local h=0

for x=b,c,17 do
    if not is_prime(x) then
        h=h+1
    end
end

local file=io.open("input.txt","r")
if file then
    print(h)
    file:close()
else
    print("Error: Unable to open input.txt")
end