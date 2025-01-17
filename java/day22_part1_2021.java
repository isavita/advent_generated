
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class ReactorReboot {

    public static void main(String[] args) throws FileNotFoundException {
        List<Step> steps = readInput("input.txt");
        long onCubes = countOnCubes(steps);
        System.out.println(onCubes);
    }

    static class Cuboid {
        int xMin, xMax, yMin, yMax, zMin, zMax;

        public Cuboid(int xMin, int xMax, int yMin, int yMax, int zMin, int zMax) {
            this.xMin = xMin;
            this.xMax = xMax;
            this.yMin = yMin;
            this.yMax = yMax;
            this.zMin = zMin;
            this.zMax = zMax;
        }

        long volume() {
            return (long) (xMax - xMin + 1) * (yMax - yMin + 1) * (zMax - zMin + 1);
        }

        Cuboid intersection(Cuboid other) {
            int xMinInt = Math.max(xMin, other.xMin);
            int xMaxInt = Math.min(xMax, other.xMax);
            int yMinInt = Math.max(yMin, other.yMin);
            int yMaxInt = Math.min(yMax, other.yMax);
            int zMinInt = Math.max(zMin, other.zMin);
            int zMaxInt = Math.min(zMax, other.zMax);

            if (xMinInt <= xMaxInt && yMinInt <= yMaxInt && zMinInt <= zMaxInt) {
                return new Cuboid(xMinInt, xMaxInt, yMinInt, yMaxInt, zMinInt, zMaxInt);
            } else {
                return null;
            }
        }
    }

    static class Step {
        boolean on;
        Cuboid cuboid;

        public Step(boolean on, Cuboid cuboid) {
            this.on = on;
            this.cuboid = cuboid;
        }
    }

    static List<Step> readInput(String filename) throws FileNotFoundException {
        List<Step> steps = new ArrayList<>();
        Scanner scanner = new Scanner(new File(filename));
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            String[] parts = line.split(" ");
            boolean on = parts[0].equals("on");
            String[] coords = parts[1].split(",");
            int xMin = Integer.parseInt(coords[0].substring(coords[0].indexOf('=') + 1, coords[0].indexOf("..")));
            int xMax = Integer.parseInt(coords[0].substring(coords[0].indexOf("..") + 2));
            int yMin = Integer.parseInt(coords[1].substring(coords[1].indexOf('=') + 1, coords[1].indexOf("..")));
            int yMax = Integer.parseInt(coords[1].substring(coords[1].indexOf("..") + 2));
            int zMin = Integer.parseInt(coords[2].substring(coords[2].indexOf('=') + 1, coords[2].indexOf("..")));
            int zMax = Integer.parseInt(coords[2].substring(coords[2].indexOf("..") + 2));
            steps.add(new Step(on, new Cuboid(xMin, xMax, yMin, yMax, zMin, zMax)));
        }
        scanner.close();
        return steps;
    }

    static long countOnCubes(List<Step> steps) {
        List<Cuboid> onCuboids = new ArrayList<>();
        for (Step step : steps) {
            Cuboid current = step.cuboid;
            
            if (current.xMin > 50 || current.xMax < -50 ||
                current.yMin > 50 || current.yMax < -50 ||
                current.zMin > 50 || current.zMax < -50) {
                continue;
            }
            
            current.xMin = Math.max(current.xMin, -50);
            current.xMax = Math.min(current.xMax, 50);
            current.yMin = Math.max(current.yMin, -50);
            current.yMax = Math.min(current.yMax, 50);
            current.zMin = Math.max(current.zMin, -50);
            current.zMax = Math.min(current.zMax, 50);

            List<Cuboid> newOnCuboids = new ArrayList<>();
            if (step.on) {
                newOnCuboids.add(current);
            }

            for (Cuboid existing : onCuboids) {
                Cuboid intersection = existing.intersection(current);
                if (intersection != null) {
                    newOnCuboids.add(intersection);
                }
            }
            
            List<Cuboid> nextOnCuboids = new ArrayList<>();
            for(Cuboid existing : onCuboids){
                nextOnCuboids.add(existing);
            }
            
            for(Cuboid newCuboid : newOnCuboids){
                boolean found = false;
                for(int i = 0; i < nextOnCuboids.size(); i++){
                    Cuboid existing = nextOnCuboids.get(i);
                    if(existing.xMin == newCuboid.xMin && existing.xMax == newCuboid.xMax &&
                       existing.yMin == newCuboid.yMin && existing.yMax == newCuboid.yMax &&
                       existing.zMin == newCuboid.zMin && existing.zMax == newCuboid.zMax){
                        nextOnCuboids.remove(i);
                        found = true;
                        break;
                    }
                }
                if(!found){
                    nextOnCuboids.add(newCuboid);
                }
            }
            onCuboids = nextOnCuboids;
        }

        long count = 0;
        List<Cuboid> positive = new ArrayList<>();
        List<Cuboid> negative = new ArrayList<>();
        
        for(Step step : steps){
            Cuboid current = step.cuboid;
            
            if (current.xMin > 50 || current.xMax < -50 ||
                current.yMin > 50 || current.yMax < -50 ||
                current.zMin > 50 || current.zMax < -50) {
                continue;
            }
            
            current.xMin = Math.max(current.xMin, -50);
            current.xMax = Math.min(current.xMax, 50);
            current.yMin = Math.max(current.yMin, -50);
            current.yMax = Math.min(current.yMax, 50);
            current.zMin = Math.max(current.zMin, -50);
            current.zMax = Math.min(current.zMax, 50);
            
            List<Cuboid> nextPositive = new ArrayList<>();
            List<Cuboid> nextNegative = new ArrayList<>();
            
            for(Cuboid existing : positive){
                Cuboid intersection = existing.intersection(current);
                if(intersection != null){
                    nextNegative.add(intersection);
                }
            }
            
            for(Cuboid existing : negative){
                Cuboid intersection = existing.intersection(current);
                if(intersection != null){
                    nextPositive.add(intersection);
                }
            }
            
            if(step.on){
                nextPositive.add(current);
            }
            
            positive.addAll(nextPositive);
            negative.addAll(nextNegative);
        }
        
        for(Cuboid cuboid : positive){
            count += cuboid.volume();
        }
        
        for(Cuboid cuboid : negative){
            count -= cuboid.volume();
        }

        return count;
    }
}
