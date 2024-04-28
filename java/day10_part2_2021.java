import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class NavigationSubsystem {
    private static final int POINTS_FOR_CLOSE_PAREN = 3;
    private static final int POINTS_FOR_CLOSE_BRACKET = 57;
    private static final int POINTS_FOR_CLOSE_BRACE = 1197;
    private static final int POINTS_FOR_CLOSE_ANGLE_BRACKET = 25137;

    private static final int AUTOCOMPLETE_POINTS_FOR_CLOSE_PAREN = 1;
    private static final int AUTOCOMPLETE_POINTS_FOR_CLOSE_BRACKET = 2;
    private static final int AUTOCOMPLETE_POINTS_FOR_CLOSE_BRACE = 3;
    private static final int AUTOCOMPLETE_POINTS_FOR_CLOSE_ANGLE_BRACKET = 4;

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            List<String> lines = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }

            int syntaxErrorScore = 0;
            List<Long> autocompleteScores = new ArrayList<>();

            for (String lineString : lines) {
                Stack<Character> stack = new Stack<>();
                boolean corrupted = false;
                for (char c : lineString.toCharArray()) {
                    if (c == '(') {
                        stack.push(')');
                    } else if (c == '[') {
                        stack.push(']');
                    } else if (c == '{') {
                        stack.push('}');
                    } else if (c == '<') {
                        stack.push('>');
                    } else if (c == ')' || c == ']' || c == '}' || c == '>') {
                        char expected = stack.pop();
                        if ((c == ')' && expected != ')') ||
                                (c == ']' && expected != ']') ||
                                (c == '}' && expected != '}') ||
                                (c == '>' && expected != '>')) {
                            corrupted = true;
                            if (c == ')') {
                                syntaxErrorScore += POINTS_FOR_CLOSE_PAREN;
                            } else if (c == ']') {
                                syntaxErrorScore += POINTS_FOR_CLOSE_BRACKET;
                            } else if (c == '}') {
                                syntaxErrorScore += POINTS_FOR_CLOSE_BRACE;
                            } else if (c == '>') {
                                syntaxErrorScore += POINTS_FOR_CLOSE_ANGLE_BRACKET;
                            }
                            break;
                        }
                    }
                }

                if (!corrupted) {
                    long autocompleteScore = 0;
                    while (!stack.isEmpty()) {
                        char c = stack.pop();
                        autocompleteScore = autocompleteScore * 5;
                        if (c == ')') {
                            autocompleteScore += AUTOCOMPLETE_POINTS_FOR_CLOSE_PAREN;
                        } else if (c == ']') {
                            autocompleteScore += AUTOCOMPLETE_POINTS_FOR_CLOSE_BRACKET;
                        } else if (c == '}') {
                            autocompleteScore += AUTOCOMPLETE_POINTS_FOR_CLOSE_BRACE;
                        } else if (c == '>') {
                            autocompleteScore += AUTOCOMPLETE_POINTS_FOR_CLOSE_ANGLE_BRACKET;
                        }
                    }
                    autocompleteScores.add(autocompleteScore);
                }
            }

            System.out.println("Syntax error score: " + syntaxErrorScore);

            autocompleteScores.sort(null);
            long middleScore = autocompleteScores.get(autocompleteScores.size() / 2);
            System.out.println("Middle autocomplete score: " + middleScore);
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }
}