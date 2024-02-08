
def ingredients = []
new File("input.txt").eachLine { line ->
    def matcher = line =~ /(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)/
    def ingredient = [matcher[0][1], matcher[0][2] as int, matcher[0][3] as int, matcher[0][4] as int, matcher[0][5] as int, matcher[0][6] as int]
    ingredients.add(ingredient)
}

def calculateScore(List<int[]> ingredients, int teaspoons, int caloriesWanted) {
    def maxScore = 0
    for (int i = 0; i <= teaspoons; i++) {
        for (int j = 0; j <= teaspoons - i; j++) {
            for (int k = 0; k <= teaspoons - i - j; k++) {
                def l = teaspoons - i - j - k
                def capacity = Math.max(0, i * ingredients[0][1] + j * ingredients[1][1] + k * ingredients[2][1] + l * ingredients[3][1])
                def durability = Math.max(0, i * ingredients[0][2] + j * ingredients[1][2] + k * ingredients[2][2] + l * ingredients[3][2])
                def flavor = Math.max(0, i * ingredients[0][3] + j * ingredients[1][3] + k * ingredients[2][3] + l * ingredients[3][3])
                def texture = Math.max(0, i * ingredients[0][4] + j * ingredients[1][4] + k * ingredients[2][4] + l * ingredients[3][4])
                def calories = i * ingredients[0][5] + j * ingredients[1][5] + k * ingredients[2][5] + l * ingredients[3][5]
                
                if (calories == caloriesWanted) {
                    def score = capacity * durability * flavor * texture
                    maxScore = Math.max(maxScore, score)
                }
            }
        }
    }
    return maxScore
}

def part1 = calculateScore(ingredients, 100, Integer.MAX_VALUE)
def part2 = calculateScore(ingredients, 100, 500)

println("Part 1: $part1")
println("Part 2: $part2")
