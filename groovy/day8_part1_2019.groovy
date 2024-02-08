File file = new File("input.txt")
def input = file.text

int width = 25
int height = 6
int layerSize = width * height
int minZeroes = Integer.MAX_VALUE
int result = 0

for (int i = 0; i < input.size(); i += layerSize) {
    def layer = input.substring(i, i + layerSize)
    int zeroes = layer.findAll { it == '0' }.size()
    if (zeroes < minZeroes) {
        minZeroes = zeroes
        result = layer.findAll { it == '1' }.size() * layer.findAll { it == '2' }.size()
    }
}

println(result)