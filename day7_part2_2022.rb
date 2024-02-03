root = [""]
dirs = {}
files = {}
curr = []
s = File.open("input.txt")

s.each_line do |line|
    txt = line.split
    if txt[0] == "$"
        if txt[1] == "cd"
            if txt[2] == "/"
                curr = root
            elsif txt[2] == ".."
                curr = curr[0...-1]
            else
                curr << txt[2]
            end
            dirs[curr.join("/")] = 0
        end
    else
        if txt[0] != "dir"
            files[(curr + [txt[1]]).join("/")] = txt[0].to_i
        end
    end
end

files.each do |f, s|
    path = f.split("/")
    (1...path.length).each do |i|
        dirs[path[0...i].join("/")] += s
    end
end

sorted_sizes = dirs.values.sort
total = 70000000
want = 30000000
available = total - dirs[""]
puts sorted_sizes.bsearch { |x| x >= want - available }