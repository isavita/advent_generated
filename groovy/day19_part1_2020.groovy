
import java.nio.file.Files
import java.nio.file.Paths

def parseInput(String filePath) {
    def lines = Files.readAllLines(Paths.get(filePath))
    def rules = [:]
    def messages = []
    boolean readingMessages = false

    lines.each { line ->
        if (line.isEmpty()) {
            readingMessages = true
            return
        }
        if (!readingMessages) {
            def (key, value) = line.split(': ')
            rules[key] = value.split(' \\| ').collect { it.split(' ').collect { it.trim() } }
        } else {
            messages << line
        }
    }
    return [rules, messages]
}

def buildMatcher(rules, ruleNumber) {
    if (rules[ruleNumber][0][0].startsWith('"')) {
        return { String message, int index ->
            return (index < message.length() && message[index] == rules[ruleNumber][0][0][1]) ? [true, index + 1] : [false, index]
        }
    }

    def subRules = rules[ruleNumber]
    return { String message, int index ->
        for (def ruleSet : subRules) {
            int currentIndex = index
            boolean matched = true
            for (def rule : ruleSet) {
                def (isMatch, newIndex) = buildMatcher(rules, rule)(message, currentIndex)
                if (!isMatch) {
                    matched = false
                    break
                }
                currentIndex = newIndex
            }
            if (matched) {
                return [true, currentIndex]
            }
        }
        return [false, index]
    }
}

def countValidMessages(rules, messages) {
    def matcher = buildMatcher(rules, '0')
    return messages.count { message ->
        def (isMatch, finalIndex) = matcher(message, 0)
        isMatch && finalIndex == message.length()
    }
}

def main() {
    def (rules, messages) = parseInput('input.txt')
    def validCount = countValidMessages(rules, messages)
    println "Number of valid messages: ${validCount}"
}

main()
