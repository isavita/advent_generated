
class Day19 {

    static String buildRegex(Map<Integer, String> rules, int ruleNum) {
        def rule = rules[ruleNum]
        if (rule.startsWith('"')) {
            return rule[1] // Extract character within quotes
        }

        def parts = rule.split(" \\| ")
        def regexParts = parts.collect { part ->
            part.split(" ").collect { subRuleNum ->
                buildRegex(rules, subRuleNum.toInteger())
            }.join("")
        }
        return "(?:" + regexParts.join("|") + ")"
    }
    static String buildRegexPart2(Map<Integer, String> rules, int ruleNum) {

        def rule = rules[ruleNum]

        if (rule.startsWith('"')) {
            return rule[1]
        }
        if (ruleNum == 8) {
            return "(?:" + buildRegexPart2(rules, 42) + "+)"  // 42 | 42 8  ->  42+
        }

        if (ruleNum == 11) {
           // 42 31 | 42 11 31.  This is a context-free grammar, and cannot be represented by a regular
           // expression directly. However we are told that the recursion depth has an upper bound.
           // We'll test a few recursion limits and see which give the right answer to part 2.
            def r42 = buildRegexPart2(rules, 42)
            def r31 = buildRegexPart2(rules, 31)

            // Create patterns like 42{1}31{1} | 42{2}31{2} | 42{3}31{3} | 42{4}31{4}
            def expandedRule = (1..5).collect { n ->  //test recursion depths.
                "${r42}{${n}}${r31}{${n}}"
            }.join("|")

            return "(?:" + expandedRule + ")"
        }


        def parts = rule.split(" \\| ")
        def regexParts = parts.collect { part ->
            part.split(" ").collect { subRuleNum ->
                buildRegexPart2(rules, subRuleNum.toInteger())
            }.join("")
        }
        return "(?:" + regexParts.join("|") + ")"
    }


    static void main(String[] args) {
        def lines = new File("input.txt").readLines()
        def rules = [:]
        def messages = []
        def parsingRules = true

        for (line in lines) {
            if (line.trim().isEmpty()) {
                parsingRules = false
                continue
            }

            if (parsingRules) {
                def (ruleNumStr, ruleDef) = line.split(": ")
                rules[ruleNumStr.toInteger()] = ruleDef
            } else {
                messages << line
            }
        }

        // Part 1
        def regex = buildRegex(rules, 0)
        def matcher = java.util.regex.Pattern.compile("^${regex}\$")
        def count1 = messages.count { matcher.matcher(it).matches() }
        println "Part 1: ${count1}"

        // Part 2

        def regex2 = buildRegexPart2(rules, 0)
        def matcher2 = java.util.regex.Pattern.compile("^${regex2}\$")
        def count2 = messages.count { matcher2.matcher(it).matches() }
        println "Part 2: ${count2}"
    }
}
