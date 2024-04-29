def sslCount = 0
new File('input.txt').eachLine { line ->
    if (supportsSSL(line)) {
        sslCount++
    }
}
println sslCount

def supportsSSL(ip) {
    def insideBrackets = ip.findAll(/\[[a-z]+\]/)
    def bracketContents = insideBrackets.collect { it[1..-2] }
    def ipNoBrackets = ip.replaceAll(/\[[a-z]+\]/, '-')
    def abas = findABAs(ipNoBrackets)
    abas.any { aba ->
        def bab = "${aba[1]}${aba[0]}${aba[1]}"
        bracketContents.any { it.contains(bab) }
    }
}

def findABAs(s) {
    def abas = []
    (0..<s.size()-2).each { i ->
        if (s[i] != s[i+1] && s[i] == s[i+2]) {
            abas << s[i..i+2]
        }
    }
    abas
}