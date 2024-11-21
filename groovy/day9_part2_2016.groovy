
import java.util.regex.Matcher
import java.util.regex.Pattern

long decompressLength(String input, boolean partTwo) {
    long length = 0
    int i = 0
    while (i < input.length()) {
        if (input[i] == '(') {
            // Found a marker
            int j = i + 1
            while (input[j] != ')') j++
            Matcher matcher = Pattern.compile("(\\d+)x(\\d+)").matcher(input.substring(i + 1, j))
            matcher.find()
            int numChars = Integer.parseInt(matcher.group(1))
            int numRepeats = Integer.parseInt(matcher.group(2))
            int start = j + 1
            int end = start + numChars
            String sub = input.substring(start, end)
            length += partTwo ? decompressLength(sub, true) * numRepeats : numChars * numRepeats
            i = end
        } else {
            length++
            i++
        }
    }
    return length
}


def input = new File("input.txt").text.replaceAll("\\s+", "")

println "Part 1: ${decompressLength(input, false)}"
println "Part 2: ${decompressLength(input, true)}"

