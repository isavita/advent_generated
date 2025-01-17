
import java.io.File;
import java.io.IOException;
import java.util.*;

public class BeaconScanner {

    public static void main(String[] args) throws IOException {
        List<List<int[]>> scanners = readInput("input.txt");

        Set<String> uniqueBeacons = new HashSet<>();
        List<int[]> scannerPositions = new ArrayList<>();
        scannerPositions.add(new int[]{0, 0, 0}); 

        List<int[]> scanner0Beacons = scanners.get(0);
        for (int[] beacon : scanner0Beacons) {
            uniqueBeacons.add(Arrays.toString(beacon));
        }
        scanners.remove(0);

        while (!scanners.isEmpty()) {
            boolean foundMatch = false;
            for (int i = 0; i < scanners.size(); i++) {
                List<int[]> currentScanner = scanners.get(i);
                for (int[][] rotation : rotations) {
                    List<int[]> rotatedBeacons = new ArrayList<>();
                    for (int[] beacon : currentScanner) {
                        rotatedBeacons.add(applyRotation(beacon, rotation));
                    }

                    Map<String, Integer> offsetCounts = new HashMap<>();
                    for (int[] beacon1 : scanner0Beacons) {
                        for (int[] beacon2 : rotatedBeacons) {
                            int[] offset = subtract(beacon1, beacon2);
                            String offsetStr = Arrays.toString(offset);
                            offsetCounts.put(offsetStr, offsetCounts.getOrDefault(offsetStr, 0) + 1);
                        }
                    }

                    for (Map.Entry<String, Integer> entry : offsetCounts.entrySet()) {
                        if (entry.getValue() >= 12) {
                            int[] scannerPos = stringToIntArray(entry.getKey());
                            scannerPositions.add(scannerPos);
                            
                            for (int[] rotatedBeacon : rotatedBeacons) {
                                int[] translatedBeacon = add(rotatedBeacon, scannerPos);
                                uniqueBeacons.add(Arrays.toString(translatedBeacon));
                            }
                            scanner0Beacons.clear();
                            for(String beaconStr : uniqueBeacons){
                                scanner0Beacons.add(stringToIntArray(beaconStr));
                            }

                            scanners.remove(i);
                            foundMatch = true;
                            break;
                        }
                    }
                    if (foundMatch) break;
                }
                if (foundMatch) break;
            }
        }

        System.out.println(uniqueBeacons.size());
    }

    private static List<List<int[]>> readInput(String filename) throws IOException {
        List<List<int[]>> scanners = new ArrayList<>();
        Scanner fileScanner = new Scanner(new File(filename));
        List<int[]> currentScanner = null;

        while (fileScanner.hasNextLine()) {
            String line = fileScanner.nextLine();
            if (line.startsWith("---")) {
                if (currentScanner != null) {
                    scanners.add(currentScanner);
                }
                currentScanner = new ArrayList<>();
            } else if (!line.isEmpty()) {
                String[] coords = line.split(",");
                currentScanner.add(new int[]{
                        Integer.parseInt(coords[0]),
                        Integer.parseInt(coords[1]),
                        Integer.parseInt(coords[2])
                });
            }
        }
        scanners.add(currentScanner);
        fileScanner.close();
        return scanners;
    }

    private static int[] applyRotation(int[] point, int[][] rotation) {
        int[] result = new int[3];
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                result[i] += rotation[i][j] * point[j];
            }
        }
        return result;
    }

    private static int[] subtract(int[] a, int[] b) {
        return new int[]{a[0] - b[0], a[1] - b[1], a[2] - b[2]};
    }

    private static int[] add(int[] a, int[] b) {
        return new int[]{a[0] + b[0], a[1] + b[1], a[2] + b[2]};
    }
    
    private static int[] stringToIntArray(String str) {
        str = str.substring(1, str.length() - 1);
        String[] parts = str.split(", ");
        return new int[]{Integer.parseInt(parts[0]), Integer.parseInt(parts[1]), Integer.parseInt(parts[2])};
    }

    private static int[][][] rotations = {
            {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}},
            {{1, 0, 0}, {0, 0, -1}, {0, 1, 0}},
            {{1, 0, 0}, {0, -1, 0}, {0, 0, -1}},
            {{1, 0, 0}, {0, 0, 1}, {0, -1, 0}},
            {{0, -1, 0}, {1, 0, 0}, {0, 0, 1}},
            {{0, 0, 1}, {1, 0, 0}, {0, 1, 0}},
            {{0, 1, 0}, {1, 0, 0}, {0, 0, -1}},
            {{0, 0, -1}, {1, 0, 0}, {0, -1, 0}},
            {{-1, 0, 0}, {0, -1, 0}, {0, 0, 1}},
            {{-1, 0, 0}, {0, 0, -1}, {0, -1, 0}},
            {{-1, 0, 0}, {0, 1, 0}, {0, 0, -1}},
            {{-1, 0, 0}, {0, 0, 1}, {0, 1, 0}},
            {{0, 1, 0}, {-1, 0, 0}, {0, 0, 1}},
            {{0, 0, 1}, {-1, 0, 0}, {0, -1, 0}},
            {{0, -1, 0}, {-1, 0, 0}, {0, 0, -1}},
            {{0, 0, -1}, {-1, 0, 0}, {0, 1, 0}},
            {{0, 0, -1}, {0, 1, 0}, {1, 0, 0}},
            {{0, 1, 0}, {0, 0, 1}, {1, 0, 0}},
            {{0, 0, 1}, {0, -1, 0}, {1, 0, 0}},
            {{0, -1, 0}, {0, 0, -1}, {1, 0, 0}},
            {{0, 0, -1}, {0, -1, 0}, {-1, 0, 0}},
            {{0, -1, 0}, {0, 0, 1}, {-1, 0, 0}},
            {{0, 0, 1}, {0, 1, 0}, {-1, 0, 0}},
            {{0, 1, 0}, {0, 0, -1}, {-1, 0, 0}}
    };
}
