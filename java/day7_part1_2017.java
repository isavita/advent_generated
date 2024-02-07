
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader br = new BufferedReader(new FileReader("input.txt"));
            String line;
            Map<String, Boolean> holderMap = new HashMap<>();
            Map<String, Boolean> heldMap = new HashMap<>();
            Pattern pattern = Pattern.compile("[a-z]+");

            while ((line = br.readLine()) != null) {
                String[] names = pattern.matcher(line).results().map(m -> m.group()).toArray(String[]::new);
                String holder = names[0];
                holderMap.put(holder, true);

                if (names.length > 1) {
                    for (int i = 1; i < names.length; i++) {
                        heldMap.put(names[i], true);
                    }
                }
            }

            br.close();

            for (String holder : holderMap.keySet()) {
                if (!heldMap.containsKey(holder)) {
                    System.out.println(holder);
                    return;
                }
            }
        } catch (IOException e) {
            System.out.println("File reading error " + e);
        }
    }
}
