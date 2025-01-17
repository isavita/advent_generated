
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ParticleSwarm {

    static class Particle {
        int id;
        int[] p;
        int[] v;
        int[] a;

        public Particle(int id, int[] p, int[] v, int[] a) {
            this.id = id;
            this.p = p;
            this.v = v;
            this.a = a;
        }

        public void update() {
            v[0] += a[0];
            v[1] += a[1];
            v[2] += a[2];
            p[0] += v[0];
            p[1] += v[1];
            p[2] += v[2];
        }

        public int manhattanDistance() {
            return Math.abs(p[0]) + Math.abs(p[1]) + Math.abs(p[2]);
        }
    }

    public static void main(String[] args) {
        List<Particle> particles = readParticlesFromFile("input.txt");
        int closestParticleId = findClosestParticle(particles);
        System.out.println(closestParticleId);
    }

    private static List<Particle> readParticlesFromFile(String filename) {
        List<Particle> particles = new ArrayList<>();
        Pattern pattern = Pattern.compile("p=<(-?\\d+),(-?\\d+),(-?\\d+)>, v=<(-?\\d+),(-?\\d+),(-?\\d+)>, a=<(-?\\d+),(-?\\d+),(-?\\d+)>");
        try (Scanner scanner = new Scanner(new File(filename))) {
            int id = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                Matcher matcher = pattern.matcher(line);
                if (matcher.find()) {
                    int[] p = {Integer.parseInt(matcher.group(1)), Integer.parseInt(matcher.group(2)), Integer.parseInt(matcher.group(3))};
                    int[] v = {Integer.parseInt(matcher.group(4)), Integer.parseInt(matcher.group(5)), Integer.parseInt(matcher.group(6))};
                    int[] a = {Integer.parseInt(matcher.group(7)), Integer.parseInt(matcher.group(8)), Integer.parseInt(matcher.group(9))};
                    particles.add(new Particle(id++, p, v, a));
                }
            }
        } catch (FileNotFoundException e) {
            System.err.println("File not found: " + filename);
        }
        return particles;
    }

    private static int findClosestParticle(List<Particle> particles) {
        int iterations = 1000;
        for (int i = 0; i < iterations; i++) {
            for (Particle particle : particles) {
                particle.update();
            }
        }

        int closestId = -1;
        int minDistance = Integer.MAX_VALUE;
        for (Particle particle : particles) {
            int distance = particle.manhattanDistance();
            if (distance < minDistance) {
                minDistance = distance;
                closestId = particle.id;
            }
        }
        return closestId;
    }
}
