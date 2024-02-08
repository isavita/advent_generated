def validPasswords = 0
def inputFile = new File("input.txt")
inputFile.eachLine { line ->
    def parts = line.split(": ")
    def password = parts[1]
    
    def ruleParts = parts[0].tokenize()
    def positions = ruleParts[0].tokenize("-")*.toInteger()
    def letter = ruleParts[1]
    
    def firstPosition = positions[0] - 1
    def secondPosition = positions[1] - 1
    
    if ((password[firstPosition] == letter && password[secondPosition] != letter) || 
        (password[firstPosition] != letter && password[secondPosition] == letter)) {
        validPasswords++
    }
}

println validPasswords