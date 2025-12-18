
import java.util.*;
import java.io.*;

/**
 * Solution for Day 12: Christmas Tree Farm
 * This program reads present shapes and grid regions from input.txt and calculates 
 * how many regions can fit the listed presents by using backtracking with pruning.
 */
public class Main {

    static class Point implements Comparable<Point> {
        int x, y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public int compareTo(Point o) {
            if (this.y != o.y) return Integer.compare(this.y, o.y);
            return Integer.compare(this.x, o.x);
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof Point)) return false;
            Point p = (Point) o;
            return x == p.x && y == p.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }

    static class Orientation {
        Point[] points;
        int w, h;
    }

    static class Shape {
        List<Orientation> orientations;
        int area;

        Shape(List<String> rows) {
            List<Point> pts = new ArrayList<>();
            for (int r = 0; r < rows.size(); r++) {
                String row = rows.get(r);
                for (int c = 0; c < row.length(); c++) {
                    if (row.charAt(c) == '#') {
                        pts.add(new Point(c, r));
                    }
                }
            }
            this.area = pts.size();
            this.orientations = generateAllOrientations(pts);
        }

        /**
         * Generates up to 8 unique orientations (rotations and flips) of the shape.
         */
        private List<Orientation> generateAllOrientations(List<Point> points) {
            Set<List<Point>> unique = new HashSet<>();
            for (int i = 0; i < 8; i++) {
                List<Point> transformed = new ArrayList<>();
                for (Point p : points) {
                    int nx = 0, ny = 0;
                    switch (i) {
                        case 0: nx = p.x; ny = p.y; break;     // Identity
                        case 1: nx = p.y; ny = -p.x; break;    // Rot 90
                        case 2: nx = -p.x; ny = -p.y; break;   // Rot 180
                        case 3: nx = -p.y; ny = p.x; break;    // Rot 270
                        case 4: nx = -p.x; ny = p.y; break;    // Flip Horizontal
                        case 5: nx = p.y; ny = p.x; break;     // Flip H + Rot 90
                        case 6: nx = p.x; ny = -p.y; break;    // Flip H + Rot 180
                        case 7: nx = -p.y; ny = -p.x; break;   // Flip H + Rot 270
                    }
                    transformed.add(new Point(nx, ny));
                }
                // Normalize to top-left (0,0)
                int minX = Integer.MAX_VALUE, minY = Integer.MAX_VALUE;
                for (Point p : transformed) {
                    minX = Math.min(minX, p.x);
                    minY = Math.min(minY, p.y);
                }
                for (Point p : transformed) {
                    p.x -= minX;
                    p.y -= minY;
                }
                Collections.sort(transformed);
                unique.add(transformed);
            }
            
            List<Orientation> res = new ArrayList<>();
            for (List<Point> pts : unique) {
                Orientation o = new Orientation();
                o.points = pts.toArray(new Point[0]);
                int mw = 0, mh = 0;
                for (Point p : pts) {
                    mw = Math.max(mw, p.x);
                    mh = Math.max(mh, p.y);
                }
                o.w = mw + 1;
                o.h = mh + 1;
                res.add(o);
            }
            return res;
        }
    }

    private static int totalPossible = 0;

    /**
     * Parses a region line, collects required presents, and initiates the fitting algorithm.
     */
    static void handleRegion(String line, List<Shape> allShapes) {
        String[] parts = line.split(":");
        if (parts.length < 2) return;
        String dimPart = parts[0].trim();
        String[] dims = dimPart.split("x");
        if (dims.length < 2) return;
        int W = Integer.parseInt(dims[0]);
        int H = Integer.parseInt(dims[1]);

        String[] counts = parts[1].trim().split("\\s+");
        List<Shape> pToFit = new ArrayList<>();
        int totalRequiredArea = 0;
        for (int i = 0; i < counts.length && i < allShapes.size(); i++) {
            int qty = Integer.parseInt(counts[i]);
            for (int q = 0; q < qty; q++) {
                pToFit.add(allShapes.get(i));
                totalRequiredArea += allShapes.get(i).area;
            }
        }

        // Heuristic: Place larger area presents first to prune the search space faster.
        pToFit.sort((a, b) -> Integer.compare(b.area, a.area));

        if (solve(0, new boolean[W * H], W, H, pToFit, totalRequiredArea, W * H)) {
            totalPossible++;
        }
    }

    /**
     * Recursive backtracking search to fit all presents into the grid.
     */
    static boolean solve(int pIdx, boolean[] grid, int W, int H, List<Shape> pToFit, int remArea, int freeArea) {
        if (pIdx == pToFit.size()) return true;
        // Basic area pruning: if total area of remaining shapes > remaining free cells, impossible.
        if (remArea > freeArea) return false;

        Shape shape = pToFit.get(pIdx);
        for (Orientation orient : shape.orientations) {
            // Early skip if orientation dimensions exceed grid dimensions
            if (orient.w > W || orient.h > H) continue;
            
            for (int r = 0; r <= H - orient.h; r++) {
                nextPos:
                for (int c = 0; c <= W - orient.w; c++) {
                    // Check for overlap between shape and grid
                    for (Point p : orient.points) {
                        if (grid[(r + p.y) * W + (c + p.x)]) continue nextPos;
                    }
                    
                    // Placement
                    for (Point p : orient.points) grid[(r + p.y) * W + (c + p.x)] = true;
                    
                    // Recurse to place next present
                    if (solve(pIdx + 1, grid, W, H, pToFit, remArea - shape.area, freeArea - shape.area)) {
                        return true;
                    }
                    
                    // Backtrack
                    for (Point p : orient.points) grid[(r + p.y) * W + (c + p.x)] = false;
                }
            }
        }
        return false;
    }

    public static void main(String[] args) throws IOException {
        File inputFile = new File("input.txt");
        if (!inputFile.exists()) return;

        BufferedReader br = new BufferedReader(new FileReader(inputFile));
        List<Shape> allShapes = new ArrayList<>();
        String line = br.readLine();
        
        // Input parsing loop
        while (line != null) {
            String trimmed = line.trim();
            if (trimmed.isEmpty()) {
                line = br.readLine();
                continue;
            }

            // Region definitions (WxH: counts...)
            if (trimmed.contains("x") && trimmed.contains(":")) {
                handleRegion(trimmed, allShapes);
                line = br.readLine();
            } 
            // Shape definitions (index: rows...)
            else if (trimmed.endsWith(":")) {
                List<String> rows = new ArrayList<>();
                while (true) {
                    line = br.readLine();
                    if (line == null) break;
                    String nextTrimmed = line.trim();
                    if (nextTrimmed.isEmpty() || nextTrimmed.contains(":")) break;
                    // Grid visualizers (# or .)
                    if (nextTrimmed.contains("#") || nextTrimmed.contains(".")) {
                        rows.add(nextTrimmed);
                    } else break;
                }
                if (!rows.isEmpty()) {
                    allShapes.add(new Shape(rows));
                }
            } else {
                line = br.readLine();
            }
        }
        
        System.out.println(totalPossible);
        br.close();
    }
}

