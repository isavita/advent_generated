def num = new File("input.txt").text.toInteger()

def answer = 0
for (int i = 1; i <= num; i++) {
    answer = (answer + 2) % i
}

println answer + 1