
import java.nio.file.Files
import java.nio.file.Paths

class Scrambler {

    static String scramble(String password, List<String> instructions) {
        String current = password
        for (String instruction : instructions) {
            current = applyInstruction(current, instruction)
        }
        return current
    }

    static String applyInstruction(String password, String instruction) {
        if (instruction.startsWith("swap position")) {
            def parts = instruction.split(" ")
            int x = parts[2].toInteger()
            int y = parts[5].toInteger()
            return swapPositions(password, x, y)
        } else if (instruction.startsWith("swap letter")) {
            def parts = instruction.split(" ")
            String x = parts[2]
            String y = parts[5]
            return swapLetters(password, x, y)
        } else if (instruction.startsWith("rotate left")) {
            def parts = instruction.split(" ")
            int steps = parts[2].toInteger()
            return rotateLeft(password, steps)
        } else if (instruction.startsWith("rotate right")) {
            def parts = instruction.split(" ")
            int steps = parts[2].toInteger()
            return rotateRight(password, steps)
        } else if (instruction.startsWith("rotate based")) {
            def parts = instruction.split(" ")
            String x = parts[6]
            return rotateBasedOnLetter(password, x)
        } else if (instruction.startsWith("reverse positions")) {
            def parts = instruction.split(" ")
            int x = parts[2].toInteger()
            int y = parts[4].toInteger()
            return reversePositions(password, x, y)
        } else if (instruction.startsWith("move position")) {
            def parts = instruction.split(" ")
            int x = parts[2].toInteger()
            int y = parts[5].toInteger()
            return movePosition(password, x, y)
        } else {
            throw new IllegalArgumentException("Unknown instruction: " + instruction)
        }
    }

    static String swapPositions(String password, int x, int y) {
        StringBuilder sb = new StringBuilder(password)
        char temp = sb.charAt(x)
        sb.setCharAt(x, sb.charAt(y))
        sb.setCharAt(y, temp)
        return sb.toString()
    }

    static String swapLetters(String password, String x, String y) {
        StringBuilder sb = new StringBuilder(password)
        for (int i = 0; i < sb.length(); i++) {
            if (sb.charAt(i).toString() == x) {
                sb.setCharAt(i, y.charAt(0))
            } else if (sb.charAt(i).toString() == y) {
                sb.setCharAt(i, x.charAt(0))
            }
        }
        return sb.toString()
    }

    static String rotateLeft(String password, int steps) {
        steps %= password.length()
        return password.substring(steps) + password.substring(0, steps)
    }

    static String rotateRight(String password, int steps) {
        steps %= password.length()
        return password.substring(password.length() - steps) + password.substring(0, password.length() - steps)
    }

    static String rotateBasedOnLetter(String password, String x) {
        int index = password.indexOf(x)
        int rotate = 1 + index
        if (index >= 4) {
            rotate++
        }
        return rotateRight(password, rotate)
    }

    static String reversePositions(String password, int x, int y) {
        StringBuilder sb = new StringBuilder(password)
        String reversed = new StringBuilder(password.substring(x, y + 1)).reverse().toString()
        sb.replace(x, y + 1, reversed)
        return sb.toString()
    }

    static String movePosition(String password, int x, int y) {
        char c = password.charAt(x)
        StringBuilder sb = new StringBuilder(password)
        sb.deleteCharAt(x)
        sb.insert(y, c)
        return sb.toString()
    }

    static String unscramble(String scrambled, List<String> instructions) {
        List<String> reversedInstructions = instructions.reverse()
        String current = scrambled
        for (String instruction : reversedInstructions) {
            current = applyReverseInstruction(current, instruction)
        }
        return current
    }

    static String applyReverseInstruction(String password, String instruction) {
         if (instruction.startsWith("swap position")) {
            def parts = instruction.split(" ")
            int x = parts[2].toInteger()
            int y = parts[5].toInteger()
            return swapPositions(password, x, y)
        } else if (instruction.startsWith("swap letter")) {
            def parts = instruction.split(" ")
            String x = parts[2]
            String y = parts[5]
            return swapLetters(password, x, y)
        } else if (instruction.startsWith("rotate left")) {
            def parts = instruction.split(" ")
            int steps = parts[2].toInteger()
            return rotateRight(password, steps) // Reverse of rotate left is rotate right
        } else if (instruction.startsWith("rotate right")) {
            def parts = instruction.split(" ")
            int steps = parts[2].toInteger()
            return rotateLeft(password, steps) // Reverse of rotate right is rotate left
        } else if (instruction.startsWith("rotate based")) {
            def parts = instruction.split(" ")
            String x = parts[6]
            return reverseRotateBasedOnLetter(password, x)
        } else if (instruction.startsWith("reverse positions")) {
            def parts = instruction.split(" ")
            int x = parts[2].toInteger()
            int y = parts[4].toInteger()
            return reversePositions(password, x, y)
        } else if (instruction.startsWith("move position")) {
            def parts = instruction.split(" ")
            int x = parts[2].toInteger()
            int y = parts[5].toInteger()
            return movePositionReverse(password, x, y)
        } else {
            throw new IllegalArgumentException("Unknown instruction: " + instruction)
        }
    }
    
    static String movePositionReverse(String password, int x, int y) {
        char c = password.charAt(y)
        StringBuilder sb = new StringBuilder(password)
        sb.deleteCharAt(y)
        sb.insert(x, c)
        return sb.toString()
    }

    static String reverseRotateBasedOnLetter(String password, String x) {
        // Brute force since we don't know the original index.  Find the rotation that results in correct state
        for (int i = 0; i < password.length(); i++) {
            String test = rotateLeft(password, i)
            if (rotateBasedOnLetter(test, x) == password) {
                return test
            }
        }
        throw new IllegalStateException("Could not reverse rotate based on letter " + x)
    }


    static void main(String[] args) {
        List<String> instructions = Files.readAllLines(Paths.get("input.txt"))

        // Part 1
        String scrambled = scramble("abcdefgh", instructions)
        println "Scrambled password: " + scrambled

        // Part 2
        String unscrambled = unscramble("fbgdceah", instructions)
        println "Unscrambled password: " + unscrambled
    }
}
