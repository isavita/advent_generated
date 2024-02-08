def input = new File("input.txt").text.trim()

def sum = 0
for (int i = 0; i < input.size(); i++) {
    if (input[i] == input[(i+1) % input.size()]) {
        sum += input[i] as int
    }
}

println sum