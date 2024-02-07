
import java.io.BufferedReader;
import java.io.FileReader;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        String passcode = readPasscode("input.txt");
        int longestPathLength = findLongestPathLength(passcode);
        System.out.println(longestPathLength);
    }

    public static String readPasscode(String filename) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            String passcode = reader.readLine();
            reader.close();
            return passcode;
        } catch (Exception e) {
            throw new RuntimeException("Failed to read passcode");
        }
    }

    public static int findLongestPathLength(String passcode) {
        int longest = 0;
        List<Point> queue = new ArrayList<>();
        queue.add(new Point(0, 0, ""));
        while (!queue.isEmpty()) {
            Point point = queue.get(0);
            queue.remove(0);

            if (point.x == 3 && point.y == 3) {
                if (point.path.length() > longest) {
                    longest = point.path.length();
                }
                continue;
            }

            for (String dir : getOpenDoors(passcode, point.path)) {
                Point nextPoint = new Point(point.x, point.y, point.path + dir);
                switch (dir) {
                    case "U":
                        nextPoint.y--;
                        break;
                    case "D":
                        nextPoint.y++;
                        break;
                    case "L":
                        nextPoint.x--;
                        break;
                    case "R":
                        nextPoint.x++;
                        break;
                }

                if (nextPoint.x >= 0 && nextPoint.x < 4 && nextPoint.y >= 0 && nextPoint.y < 4) {
                    queue.add(nextPoint);
                }
            }
        }
        return longest;
    }

    public static List<String> getOpenDoors(String passcode, String path) {
        String hash = md5Hash(passcode + path);
        List<String> doors = new ArrayList<>();
        if (hash.charAt(0) >= 'b' && hash.charAt(0) <= 'f') {
            doors.add("U");
        }
        if (hash.charAt(1) >= 'b' && hash.charAt(1) <= 'f') {
            doors.add("D");
        }
        if (hash.charAt(2) >= 'b' && hash.charAt(2) <= 'f') {
            doors.add("L");
        }
        if (hash.charAt(3) >= 'b' && hash.charAt(3) <= 'f') {
            doors.add("R");
        }
        return doors;
    }

    public static String md5Hash(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            md.update(input.getBytes());
            byte[] digest = md.digest();
            StringBuilder sb = new StringBuilder();
            for (byte b : digest) {
                sb.append(String.format("%02x", b & 0xff));
            }
            return sb.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("MD5 algorithm not found");
        }
    }

    static class Point {
        int x, y;
        String path;

        public Point(int x, int y, String path) {
            this.x = x;
            this.y = y;
            this.path = path;
        }
    }
}
