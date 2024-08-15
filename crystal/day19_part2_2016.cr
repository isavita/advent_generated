require "file_utils"

class LLNode
  property elf_num : Int32
  property presents : Int32
  property next : LLNode?
  def initialize(@elf_num : Int32, @presents : Int32)
  end
end

def elephant(input : String) : Int32
  starting_elves = input.to_i
  root = LLNode.new(1, 1)
  iter = root
  (2..starting_elves).each do |i|
    new_node = LLNode.new(i, 1)
    iter.next = new_node
    iter = new_node
  end
  iter.next = root

  is_odd_length = starting_elves % 2 == 1
  before_across = root
  (starting_elves // 2 - 1).times do
    before_across = before_across.next.not_nil!
  end

  while root.next != root
    root.presents += before_across.next.not_nil!.presents
    before_across.next = before_across.next.not_nil!.next
    if is_odd_length
      before_across = before_across.next.not_nil!
    end
    is_odd_length = !is_odd_length
    root = root.next.not_nil!
  end

  root.elf_num
end

def read_file(path : String) : String
  File.read(path).strip
end

input = read_file("input.txt")
puts elephant(input)