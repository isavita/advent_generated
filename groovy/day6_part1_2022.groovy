
def s = new File("input.txt").text.trim()
println firstNUnique(s, 4)

def firstNUnique(s, n) {
    for (int i = n; i < s.length(); i++) {
        def b = s.substring(i - n, i).toList()
        if (b.size() == b.toSet().size()) {
            return i
        }
    }
    return -1
}
