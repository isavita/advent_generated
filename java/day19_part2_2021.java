
import java.io.*;
import java.util.*;

public class Solution {
    static int[][][] rotations;

    static {
        generateRotations();
    }

    public static List<List<int[]>> readInput(String filename) throws IOException {
        List<List<int[]>> scanners = new ArrayList<>();
        BufferedReader br = new BufferedReader(new FileReader(filename));
        String line;
        List<int[]> scanner = null;
        while ((line = br.readLine()) != null) {
            line = line.trim();
            if (line.startsWith("---")) {
                if (scanner != null) {
                    scanners.add(scanner);
                }
                scanner = new ArrayList<>();
            } else if (!line.isEmpty()) {
                String[] parts = line.split(",");
                int[] coords = new int[3];
                for (int i = 0; i < 3; i++) {
                    coords[i] = Integer.parseInt(parts[i]);
                }
                scanner.add(coords);
            }
        }
        if (scanner != null) {
            scanners.add(scanner);
        }
        br.close();
        return scanners;
    }

    public static void generateRotations() {
        rotations = new int[24][3][3];
        int[][][] baseRotations = {
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
        System.arraycopy(baseRotations, 0, rotations, 0, 24);
    }

    public static int[] rotate(int[] p, int[][] rot) {
        int[] res = new int[3];
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                res[i] += p[j] * rot[i][j];
            }
        }
        return res;
    }

    public static int[] add(int[] p1, int[] p2) {
        int[] res = new int[3];
        for (int i = 0; i < 3; i++) {
            res[i] = p1[i] + p2[i];
        }
        return res;
    }

    public static int[] subtract(int[] p1, int[] p2) {
        int[] res = new int[3];
        for (int i = 0; i < 3; i++) {
            res[i] = p1[i] - p2[i];
        }
        return res;
    }

    public static int manhattan(int[] p1, int[] p2) {
        int dist = 0;
        for (int i = 0; i < 3; i++) {
            dist += Math.abs(p1[i] - p2[i]);
        }
        return dist;
    }

    public static int solve(List<List<int[]>> scanners) {
        Set<Integer> aligned = new HashSet<>();
        aligned.add(0);
        Map<Integer, int[]> scannerPositions = new HashMap<>();
        scannerPositions.put(0, new int[]{0, 0, 0});
        Set<String> beacons = new HashSet<>();
        for (int[] p : scanners.get(0)) {
            beacons.add(p[0] + "," + p[1] + "," + p[2]);
        }
        Set<Integer> pending = new HashSet<>();
        for (int i = 1; i < scanners.size(); i++) {
            pending.add(i);
        }

        while (!pending.isEmpty()) {
            boolean found = false;
            Iterator<Integer> iter = pending.iterator();
            while (iter.hasNext()) {
                int scanner = iter.next();
                for (int[][] rot : rotations) {
                    List<int[]> rotated = new ArrayList<>();
                    for (int[] p : scanners.get(scanner)) {
                        rotated.add(rotate(p, rot));
                    }
                    Map<String, Integer> deltas = new HashMap<>();
                    int[] maxDelta = null;
                    int maxCount = 0;

                    for (int[] beacon : rotated) {
                        for (String alignedBeaconStr : beacons) {
                            String[] parts = alignedBeaconStr.split(",");
                            int[] alignedBeacon = new int[]{Integer.parseInt(parts[0]), Integer.parseInt(parts[1]), Integer.parseInt(parts[2])};
                            int[] delta = subtract(alignedBeacon, beacon);
                            String deltaStr = delta[0] + "," + delta[1] + "," + delta[2];
                            int count = deltas.getOrDefault(deltaStr, 0) + 1;
                            deltas.put(deltaStr, count);
                            if (count > maxCount) {
                                maxCount = count;
                                maxDelta = delta;
                            }
                        }
                    }

                    if (maxCount >= 12) {
                        scannerPositions.put(scanner, maxDelta);
                        for (int[] beacon : rotated) {
                            int[] translated = add(beacon, maxDelta);
                            beacons.add(translated[0] + "," + translated[1] + "," + translated[2]);
                        }
                        aligned.add(scanner);
                        iter.remove();
                        found = true;
                        break;
                    }
                }
                if (found) {
                    break;
                }
            }
        }
        int maxDistance = 0;
        List<int[]> positions = new ArrayList<>(scannerPositions.values());
        for (int i = 0; i < positions.size(); i++) {
            for (int j = i + 1; j < positions.size(); j++) {
                int dist = manhattan(positions.get(i), positions.get(j));
                maxDistance = Math.max(maxDistance, dist);
            }
        }
        return maxDistance;
    }

    public static void main(String[] args) throws IOException {
        List<List<int[]>> scanners = readInput("input.txt");
        int result = solve(scanners);
        System.out.println(result);
    }
}
