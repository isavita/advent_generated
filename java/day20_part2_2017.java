
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class ParticleSwarm {

    static class Vector {
        int x, y, z;

        public Vector(int x, int y, int z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Vector vector = (Vector) o;
            return x == vector.x && y == vector.y && z == vector.z;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y, z);
        }
    }

    static class Particle {
        Vector position;
        Vector velocity;
        Vector acceleration;

        public Particle(Vector position, Vector velocity, Vector acceleration) {
            this.position = position;
            this.velocity = velocity;
            this.acceleration = acceleration;
        }

        public void update() {
            velocity.x += acceleration.x;
            velocity.y += acceleration.y;
            velocity.z += acceleration.z;
            position.x += velocity.x;
            position.y += velocity.y;
            position.z += velocity.z;
        }

        public int manhattanDistance() {
            return Math.abs(position.x) + Math.abs(position.y) + Math.abs(position.z);
        }
    }

    public static void main(String[] args) {
        List<Particle> particles = readParticlesFromFile("input.txt");

        // Part 1
        int closestParticleIndex = findClosestParticle(particles);
        System.out.println(closestParticleIndex);

        // Part 2
        int remainingParticles = simulateCollisions(new ArrayList<>(particles));
        System.out.println(remainingParticles);
    }

    private static List<Particle> readParticlesFromFile(String filename) {
        List<Particle> particles = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(", ");
                Vector position = parseVector(parts[0].substring(3, parts[0].length() - 1));
                Vector velocity = parseVector(parts[1].substring(3, parts[1].length() - 1));
                Vector acceleration = parseVector(parts[2].substring(3, parts[2].length() - 1));
                particles.add(new Particle(position, velocity, acceleration));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return particles;
    }

    private static Vector parseVector(String vectorString) {
        String[] coords = vectorString.split(",");
        return new Vector(Integer.parseInt(coords[0]), Integer.parseInt(coords[1]), Integer.parseInt(coords[2]));
    }

    private static int findClosestParticle(List<Particle> particles) {
        int closestIndex = 0;
        int minDistance = Integer.MAX_VALUE;
        for (int i = 0; i < particles.size(); i++) {
            Particle particle = particles.get(i);
            int distance = calculateLongTermDistance(particle);
            if (distance < minDistance) {
                minDistance = distance;
                closestIndex = i;
            }
        }
        return closestIndex;
    }

    private static int calculateLongTermDistance(Particle particle) {
        int absAcc = Math.abs(particle.acceleration.x) + Math.abs(particle.acceleration.y) + Math.abs(particle.acceleration.z);
        int absVel = Math.abs(particle.velocity.x) + Math.abs(particle.velocity.y) + Math.abs(particle.velocity.z);
        int absPos = Math.abs(particle.position.x) + Math.abs(particle.position.y) + Math.abs(particle.position.z);
        
        if (absAcc > 0) {
            return absAcc;
        } else if (absVel > 0) {
            return absVel;
        } else {
            return absPos;
        }
    }

    private static int simulateCollisions(List<Particle> particles) {
        for (int i = 0; i < 1000; i++) { // Simulate for a large number of ticks
            Map<Vector, List<Integer>> positions = new HashMap<>();
            for (int j = 0; j < particles.size(); j++) {
                Particle particle = particles.get(j);
                particle.update();
                positions.computeIfAbsent(particle.position, k -> new ArrayList<>()).add(j);
            }

            List<Integer> toRemove = new ArrayList<>();
            for (List<Integer> indices : positions.values()) {
                if (indices.size() > 1) {
                    toRemove.addAll(indices);
                }
            }
            
            // Remove particles in reverse order to avoid index issues
            toRemove.sort((a, b) -> b - a);
            for (int index : toRemove) {
                particles.remove(index);
            }
        }
        return particles.size();
    }
}
