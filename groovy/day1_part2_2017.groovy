def input = new File('input.txt').text.trim()
def halfway = (int) (input.size() / 2)
def sum = 0

for (int i = 0; i < input.size(); i++) {
    def next = (i + halfway) % input.size()
    if (input.charAt(i) == input.charAt(next)) {
        sum += Character.digit(input.charAt(i), 10)
    }
}

println sum