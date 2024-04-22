require "file_utils"

class Node
  property name : String
  property children : Array(Node)
  property parent : Node?

  def initialize(@name : String)
    @children = [] of Node
  end
end

def find_or_create_node(name : String, nodes : Hash(String, Node)) : Node
  nodes[name] ||= Node.new(name)
end

def build_orbit_map(file : File) : Hash(String, Node)
  nodes = {} of String => Node
  file.each_line do |line|
    center_name, orbiter_name = line.chomp.split(")")
    center = find_or_create_node(center_name, nodes)
    orbiter = find_or_create_node(orbiter_name, nodes)
    center.children << orbiter
    orbiter.parent = center
  end
  nodes
end

def path_to_root(node : Node) : Array(Node)
  path = [] of Node
  while node
    path << node
    node = node.parent
  end
  path
end

def find_common_ancestor(node1 : Node, node2 : Node) : Tuple(Int32, Int32)
  path1 = path_to_root(node1)
  path2 = path_to_root(node2)

  i = path1.size - 1
  j = path2.size - 1

  while i >= 0 && j >= 0 && path1[i] == path2[j]
    i -= 1
    j -= 1
  end
  {i + 1, j + 1}
end

file = File.open("input.txt")
orbit_map = build_orbit_map(file)

transfers_you, transfers_san = find_common_ancestor(orbit_map["YOU"].parent.not_nil!, orbit_map["SAN"].parent.not_nil!)
puts transfers_you + transfers_san