def validPasswords = 0
new File('input.txt').eachLine { line ->
    def (policy, password) = line.split(': ')
    def (range, letter) = policy.tokenize(' ')
    def (min, max) = range.tokenize('-')*.toInteger()
    
    def count = password.count(letter)
    if (count >= min && count <= max) {
        validPasswords++
    }
}
println validPasswords