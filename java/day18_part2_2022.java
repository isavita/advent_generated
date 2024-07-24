
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;

public class Main {
    static class Pt3 {
        int x, y, z;

        Pt3(int x, int y, int z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        Pt3 add(Pt3 p) {
            return new Pt3(this.x + p.x, this.y + p.y, this.z + p.z);
        }
    }

    public static void main(String[] args) throws IOException {
        Map<String, Boolean> cubes = new HashMap<>();
        int[] min = {Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE};
        int[] max = {Integer.MIN_VALUE, Integer.MIN_VALUE, Integer.MIN_VALUE};
        String line;

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            while ((line = br.readLine()) != null && !line.isEmpty()) {
                String[] parts = line.split(",");
                Pt3 cube = new Pt3(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]), Integer.parseInt(parts[2]));
                cubes.put(cube.x + "," + cube.y + "," + cube.z, true);
                min[0] = Math.min(min[0], cube.x);
                min[1] = Math.min(min[1], cube.y);
                min[2] = Math.min(min[2], cube.z);
                max[0] = Math.max(max[0], cube.x);
                max[1] = Math.max(max[1], cube.y);
                max[2] = Math.max(max[2], cube.z);
            }
        }

        for (int i = 0; i < 3; i++) {
            min[i]--;
            max[i]++;
        }

        int faces = 0;
        Queue<Pt3> queue = new LinkedList<>();
        Map<String, Boolean> seen = new HashMap<>();
        Pt3 start = new Pt3(min[0], min[1], min[2]);
        queue.add(start);
        seen.put(start.x + "," + start.y + "," + start.z, true);

        int[][] neighbors = {{-1, 0, 0}, {1, 0, 0}, {0, -1, 0}, {0, 1, 0}, {0, 0, -1}, {0, 0, 1}};

        while (!queue.isEmpty()) {
            Pt3 curr = queue.poll();
            for (int[] delta : neighbors) {
                Pt3 next = curr.add(new Pt3(delta[0], delta[1], delta[2]));
                if (next.x < min[0] || next.y < min[1] || next.z < min[2] || 
                    next.x > max[0] || next.y > max[1] || next.z > max[2]) {
                    continue;
                }
                if (cubes.containsKey(next.x + "," + next.y + "," + next.z)) {
                    faces++;
                } else if (!seen.containsKey(next.x + "," + next.y + "," + next.z)) {
                    seen.put(next.x + "," + next.y + "," + next.z, true);
                    queue.add(next);
                }
            }
        }
        System.out.println(faces);
    }
}
