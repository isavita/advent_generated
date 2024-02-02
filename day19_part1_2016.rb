
elves = File.read('input.txt').to_i
def josephus(elves)
  2 * (elves - 2 ** (Math.log(elves, 2).to_i)) + 1
end
puts josephus(elves)
