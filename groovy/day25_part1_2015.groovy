
import java.util.regex.Matcher
import java.util.regex.Pattern

def input = new File("input.txt").text

def pattern = Pattern.compile("row (\\d+), column (\\d+)")
def matcher = pattern.matcher(input)

if (!matcher.find()) {
    throw new IllegalArgumentException("Invalid input format.")
}

def row = Integer.parseInt(matcher.group(1))
def col = Integer.parseInt(matcher.group(2))

def pos = getPosition(row, col)
def code = getCode(pos)

println code


long getPosition(int row, int col) {
    return (row + col - 2) * (row + col - 1) / 2 + col
}

long getCode(long position) {
    long startCode = 20151125
    long multiplier = 252533
    long modulus = 33554393

    long code = startCode
    (1..<position).each {
        code = (code * multiplier) % modulus
    }
    return code
}
