d=open("input.txt","r")
l=readlines(d)
close(d)

function priority(c)
    if 'a'<=c<='z'
        return Int(c)-Int('a')+1
    else
        return Int(c)-Int('A')+27
    end
end

function part1()
    s=0
    for line in l
        n=length(line)÷2
        c1=Set(line[1:n])
        c2=Set(line[n+1:end])
        s+=priority(first(intersect(c1,c2)))
    end
    return s
end

function part2()
    s=0
    for i in 1:3:length(l)
        c1=Set(l[i])
        c2=Set(l[i+1])
        c3=Set(l[i+2])
        s+=priority(first(intersect(c1,c2,c3)))
    end
    return s
end

println(part1())
println(part2())