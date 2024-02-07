
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader br = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = br.readLine()) != null) {
                if (isRealRoom(line)) {
                    String decryptedName = decryptName(line);
                    if (decryptedName.contains("northpole object")) {
                        System.out.println(getSectorID(line));
                        break;
                    }
                }
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static boolean isRealRoom(String room) {
        String[] parts = room.split("\\[");
        String checksum = parts[1].replace("]", "");
        String[] encryptedName = parts[0].split("-");
        encryptedName = new ArrayList<>(List.of(encryptedName).subList(0, encryptedName.length - 1)).toArray(new String[0]);

        Map<Character, Integer> letterCounts = new HashMap<>();
        for (String part : encryptedName) {
            for (char letter : part.toCharArray()) {
                letterCounts.put(letter, letterCounts.getOrDefault(letter, 0) + 1);
            }
        }

        List<Map.Entry<Character, Integer>> counts = new ArrayList<>(letterCounts.entrySet());
        Collections.sort(counts, (a, b) -> a.getValue().equals(b.getValue()) ? a.getKey().compareTo(b.getKey()) : b.getValue().compareTo(a.getValue()));

        for (int i = 0; i < checksum.length(); i++) {
            if (checksum.charAt(i) != counts.get(i).getKey()) {
                return false;
            }
        }

        return true;
    }

    public static int getSectorID(String room) {
        String[] parts = room.split("-");
        String sectorIDPart = parts[parts.length - 1];
        int sectorID = Integer.parseInt(sectorIDPart.split("\\[")[0]);
        return sectorID;
    }

    public static String decryptName(String room) {
        String[] parts = room.split("-");
        String sectorIDPart = parts[parts.length - 1];
        int sectorID = Integer.parseInt(sectorIDPart.split("\\[")[0]);
        StringBuilder decryptedName = new StringBuilder();

        for (String part : new ArrayList<>(List.of(parts).subList(0, parts.length - 1))) {
            for (char letter : part.toCharArray()) {
                if (letter == '-') {
                    decryptedName.append(' ');
                } else {
                    char shiftedLetter = (char)('a' + ((letter - 'a' + sectorID) % 26));
                    decryptedName.append(shiftedLetter);
                }
            }
            decryptedName.append(' ');
        }

        return decryptedName.toString().trim();
    }
}
