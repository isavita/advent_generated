
import "io" for File

class Ingredient {
  construct new(name, capacity, durability, flavor, texture) {
    _name = name
    _capacity = capacity
    _durability = durability
    _flavor = flavor
    _texture = texture
  }
  capacity { _capacity }
  durability { _durability }
  flavor { _flavor }
  texture { _texture }
}

var readIngredients = Fn.new { |filename|
  var ingredients = []
  var lines = File.read(filename).split("\n")
  for (line in lines) {
    var parts = line.split(" ")
    if (parts.count < 11) continue
    var capacity = Num.fromString(parts[2][0..-2])
    var durability = Num.fromString(parts[4][0..-2])
    var flavor = Num.fromString(parts[6][0..-2])
    var texture = Num.fromString(parts[8][0..-2])
    ingredients.add(Ingredient.new(parts[0], capacity, durability, flavor, texture))
  }
  return ingredients
}

var score = Fn.new { |ingredients, teaspoons|
  var capacity = 0
  var durability = 0
  var flavor = 0
  var texture = 0
  for (i in 0...ingredients.count) {
    capacity = capacity + ingredients[i].capacity * teaspoons[i]
    durability = durability + ingredients[i].durability * teaspoons[i]
    flavor = flavor + ingredients[i].flavor * teaspoons[i]
    texture = texture + ingredients[i].texture * teaspoons[i]
  }
  capacity = capacity < 0 ? 0 : capacity
  durability = durability < 0 ? 0 : durability
  flavor = flavor < 0 ? 0 : flavor
  texture = texture < 0 ? 0 : texture
  return capacity * durability * flavor * texture
}

var findMaxScore = Fn.new { |ingredients, totalTeaspoons|
  var calculateMaxScore
  calculateMaxScore = Fn.new { |ingredients, index, remaining, teaspoons|
    if (index == ingredients.count - 1) {
      teaspoons.add(remaining)
      var result = score.call(ingredients, teaspoons)
      teaspoons.removeAt(teaspoons.count-1)
      return result
    }
    var maxScore = 0
    for (i in 0..remaining) {
      teaspoons.add(i)
      var currentScore = calculateMaxScore.call(ingredients, index + 1, remaining - i, teaspoons)
      teaspoons.removeAt(teaspoons.count-1)
      if (currentScore > maxScore) {
        maxScore = currentScore
      }
    }
    return maxScore
  }
  return calculateMaxScore.call(ingredients, 0, totalTeaspoons, [])
}

var ingredients = readIngredients.call("input.txt")
var maxScore = findMaxScore.call(ingredients, 100)
System.print(maxScore)
