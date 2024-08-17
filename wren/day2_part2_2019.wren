import "io" for File

var execute = Fn.new { |memory|
  var i = 0
  while (i < memory.count) {
    if (memory[i] == 1) {
      memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]]
      i = i + 4
    } else if (memory[i] == 2) {
      memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]]
      i = i + 4
    } else if (memory[i] == 99) {
      return memory[0]
    } else {
      i = i + 4
    }
  }
  return memory[0]
}

var main = Fn.new {
  var input = File.read("input.txt")
  var strs = input.trim().split(",")
  var original = []
  for (s in strs) {
    original.add(Num.fromString(s))
  }

  for (noun in 0..99) {
    for (verb in 0..99) {
      var memory = List.filled(original.count, 0)
      for (i in 0..original.count - 1) {
        memory[i] = original[i]
      }
      memory[1] = noun
      memory[2] = verb
      if (execute.call(memory) == 19690720) {
        System.print(100 * noun + verb)
        return
      }
    }
  }
}

main.call()