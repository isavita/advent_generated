
import java.util.regex.Matcher
import java.util.regex.Pattern

def input = new File("input.txt").text

def pattern = Pattern.compile("mul\\((\\d+),\\s*(\\d+)\\)")
def matcher = pattern.matcher(input)

long sum = 0
while (matcher.find()) {
    long num1 = Long.parseLong(matcher.group(1))
    long num2 = Long.parseLong(matcher.group(2))
    sum += num1 * num2
}

println sum
