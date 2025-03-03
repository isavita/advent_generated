
import java.nio.file.Files
import java.nio.file.Paths

class LensLibrary {

    static int hash(String s) {
        int currentValue = 0
        for (char c : s.toCharArray()) {
            currentValue += (int) c
            currentValue *= 17
            currentValue %= 256
        }
        return currentValue
    }

    static void main(String[] args) {
        String input = Files.readString(Paths.get("input.txt")).trim()
        List<String> steps = input.split(",")

        // Part 1
        int sumOfHashes = steps.sum { hash(it) }
        println "Part 1: Sum of HASH results = ${sumOfHashes}"

        // Part 2
        List<List<List<String>>> boxes = new ArrayList<>()
        for (int i = 0; i < 256; i++) {
            boxes.add(new ArrayList<>())
        }

        steps.each { step ->
            if (step.contains('=')) {
                String[] parts = step.split("=")
                String label = parts[0]
                int focalLength = Integer.parseInt(parts[1])
                int boxIndex = hash(label)
                List<List<String>> box = boxes.get(boxIndex)

                boolean replaced = false
                for (int i = 0; i < box.size(); i++) {
                    if (box.get(i).get(0) == label) {
                        box.set(i, [label, String.valueOf(focalLength)])
                        replaced = true
                        break
                    }
                }

                if (!replaced) {
                    box.add([label, String.valueOf(focalLength)])
                }
            } else if (step.contains('-')) {
                String label = step.substring(0, step.length() - 1)
                int boxIndex = hash(label)
                List<List<String>> box = boxes.get(boxIndex)
                box.removeIf { it.get(0) == label }
            }
        }

        long focusingPower = 0
        for (int i = 0; i < boxes.size(); i++) {
            List<List<String>> box = boxes.get(i)
            for (int j = 0; j < box.size(); j++) {
                focusingPower += (long) (i + 1) * (j + 1) * Integer.parseInt(box.get(j).get(1))
            }
        }

        println "Part 2: Total focusing power = ${focusingPower}"
    }
}
