
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.HashMap;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            HashMap<String, HashMap<Character, String>> keypad = new HashMap<>();
            keypad.put("1", new HashMap<Character, String>() {{ put('D', "3"); }});
            keypad.put("2", new HashMap<Character, String>() {{ put('R', "3"); put('D', "6"); }});
            keypad.put("3", new HashMap<Character, String>() {{ put('U', "1"); put('R', "4"); put('D', "7"); put('L', "2"); }});
            keypad.put("4", new HashMap<Character, String>() {{ put('L', "3"); put('D', "8"); }});
            keypad.put("5", new HashMap<Character, String>() {{ put('R', "6"); }});
            keypad.put("6", new HashMap<Character, String>() {{ put('U', "2"); put('R', "7"); put('D', "A"); put('L', "5"); }});
            keypad.put("7", new HashMap<Character, String>() {{ put('U', "3"); put('R', "8"); put('D', "B"); put('L', "6"); }});
            keypad.put("8", new HashMap<Character, String>() {{ put('U', "4"); put('R', "9"); put('D', "C"); put('L', "7"); }});
            keypad.put("9", new HashMap<Character, String>() {{ put('L', "8"); }});
            keypad.put("A", new HashMap<Character, String>() {{ put('U', "6"); put('R', "B"); }});
            keypad.put("B", new HashMap<Character, String>() {{ put('U', "7"); put('R', "C"); put('D', "D"); put('L', "A"); }});
            keypad.put("C", new HashMap<Character, String>() {{ put('U', "8"); put('L', "B"); }});
            keypad.put("D", new HashMap<Character, String>() {{ put('U', "B"); }});

            String position = "5"; // Start at '5'
            String code = "";

            while (scanner.hasNextLine()) {
                String instruction = scanner.nextLine();
                for (char move : instruction.toCharArray()) {
                    String nextPos = keypad.get(position).get(move);
                    position = nextPos != null ? nextPos : position;
                }
                code += position;
            }

            System.out.println(code);
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }
}
