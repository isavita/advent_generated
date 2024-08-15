require "file_utils"

struct Node
  def initialize(@used : Int32, @avail : Int32)
  end

  def used
    @used
  end

  def avail
    @avail
  end
end

def read_nodes(filename : String) : Array(Node)
  nodes = [] of Node
  File.open(filename, "r") do |file|
    file.each_line do |line|
      if match = line.match(/node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%/)
        used = match[1].to_i
        avail = match[2].to_i
        nodes << Node.new(used: used, avail: avail)
      end
    end
  end
  nodes
end

def count_viable_pairs(nodes : Array(Node)) : Int32
  count = 0
  nodes.each_with_index do |a, i|
    nodes.each_with_index do |b, j|
      if i != j && a.used > 0 && a.used <= b.avail
        count += 1
      end
    end
  end
  count
end

nodes = read_nodes("input.txt")
puts count_viable_pairs(nodes)