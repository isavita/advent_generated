
import java.util.regex.Matcher
import java.util.regex.Pattern

int hashString(String str) {
    int res = 0
    for (int i = 0; i < str.length(); i++) {
        char c = str.charAt(i)
        res += (int) c
        res *= 17
        res %= 256
    }
    return res
}

int solve() {
    File inputFile = new File("input.txt")
    String line = inputFile.text.trim()
    String[] steps = line.split(",")
    int res = 0
    for (String step : steps) {
        res += hashString(step.trim())
    }
    return res
}

println solve()
