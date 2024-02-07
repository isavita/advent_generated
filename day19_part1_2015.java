
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            List<String> replacements = new ArrayList<>();
            String molecule = "";
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                if (line.equals("")) {
                    continue;
                }
                if (line.contains(" => ")) {
                    replacements.add(line);
                } else {
                    molecule = line;
                }
            }

            Map<String, Boolean> molecules = new HashMap<>();
            for (String replacement : replacements) {
                String[] parts = replacement.split(" => ");
                for (int i = 0; i < molecule.length(); i++) {
                    if (molecule.substring(i).startsWith(parts[0])) {
                        String newMolecule = molecule.substring(0, i) + parts[1] + molecule.substring(i + parts[0].length());
                        molecules.put(newMolecule, true);
                    }
                }
            }

            System.out.println(molecules.size());
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
