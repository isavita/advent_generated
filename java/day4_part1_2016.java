
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
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            int sumOfSectorIDs = 0;

            while ((line = reader.readLine()) != null) {
                if (isRealRoom(line)) {
                    sumOfSectorIDs += getSectorID(line);
                }
            }

            System.out.println(sumOfSectorIDs);
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static boolean isRealRoom(String room) {
        String[] parts = room.split("\\[");
        String checksum = parts[1].replace("]", "");
        String[] encryptedName = parts[0].split("-");
        List<Character> encryptedChars = new ArrayList<>();

        for (int i = 0; i < encryptedName.length - 1; i++) {
            char[] chars = encryptedName[i].toCharArray();
            for (char c : chars) {
                encryptedChars.add(c);
            }
        }

        Map<Character, Integer> letterCounts = new HashMap<>();
        for (char c : encryptedChars) {
            letterCounts.put(c, letterCounts.getOrDefault(c, 0) + 1);
        }

        List<Map.Entry<Character, Integer>> counts = new ArrayList<>(letterCounts.entrySet());
        counts.sort((a, b) -> {
            if (a.getValue().equals(b.getValue())) {
                return a.getKey().compareTo(b.getKey());
            }
            return b.getValue().compareTo(a.getValue());
        });

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
}
