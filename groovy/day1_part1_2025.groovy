
class DialCounter {
    static void main(String[] args) {
        int pos = 50, zero = 0
        new File('input.txt').eachLine { l ->
            l = l.trim()
            if (!l) return
            def dir = l[0]
            int amt = l[1..-1].toInteger()
            if (dir == 'R') pos = (pos + amt) % 100
            else if (dir == 'L') pos = Math.floorMod(pos - amt, 100)
            else throw new IllegalArgumentException("Invalid direction $dir")
            if (pos == 0) zero++
        }
        println "The password is: $zero"
    }
}
