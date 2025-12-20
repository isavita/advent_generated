
class Main {
    static void main(String[] args) {
        def points = []
        new File('input.txt').eachLine { line ->
            line = line.trim()
            if (!line) return
            def parts = line.split(',')
            if (parts.size() != 2) return
            def x = parts[0] as int
            def y = parts[1] as int
            points << [x, y]
        }
        long maxArea = 0
        int n = points.size()
        for (int i = 0; i < n; i++) {
            for (int j = i; j < n; j++) {
                def p1 = points[i]
                def p2 = points[j]
                long dx = Math.abs(p1[0] - p2[0]) + 1
                long dy = Math.abs(p1[1] - p2[1]) + 1
                long area = dx * dy
                if (area > maxArea) maxArea = area
            }
        }
        println "Largest area: $maxArea"
    }
}
