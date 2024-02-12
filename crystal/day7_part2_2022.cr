
def to_int(s : String) : Int32
  s.to_i
end

root = [""]
dirs = Hash(String, Int32).new
files = Hash(Array(String), Int32).new
curr = [] of String

File.open("input.txt") do |file|
  file.each_line do |line|
    txt = line.split
    if txt[0] == "$"
      if txt[1] == "cd"
        if txt[2] == "/"
          curr = root
        elsif txt[2] == ".."
          curr = curr[0, curr.size - 1]
        else
          curr << txt[2]
        end
        dirs[curr.join("/")] = 0
      end
    else
      unless txt[0] == "dir"
        files[curr + [txt[1]]] = to_int(txt[0])
      end
    end
  end
end

files.each do |f, s|
  path = f
  (1...path.size).each do |i|
    dirs[path[0, i].join("/")] += s
  end
end

sorted_sizes = dirs.values.sort

total = 70000000
want = 30000000
available = total - dirs[""]
puts sorted_sizes.bsearch { |x| x >= want - available }
