import java.util.*;

class Main {
    static final int MAX_LEVEL = 50
    static final int MAX_QUEUE_SIZE = 1000000

    static class PortalLink {
        int toX, toY
        boolean isOuter
        PortalLink(int tx, int ty, boolean o) { toX = tx; toY = ty; isOuter = o }
    }

    static class FoundPortal {
        String label
        int x, y
        FoundPortal(String l, int x, int y) { label = l; this.x = x; this.y = y }
    }

    static boolean isOuter(int x, int y, int width, int height) {
        return x <= 2 || y <= 2 || x >= width - 3 || y >= height - 3
    }

    static int key(int x, int y, int width) {
        return y * width + x
    }

    static void main(String[] args) {
        List<String> lines = new File("input.txt").readLines()
        int height = lines.size()
        int width = lines.collect { it.length() }.max() ?: 0

        char[][] grid = new char[height][width]
        for (int y = 0; y < height; y++) {
            char[] row = lines[y].toCharArray()
            for (int x = 0; x < width; x++) {
                grid[y][x] = (x < row.length) ? row[x] : ' '
            }
        }

        List<FoundPortal> foundList = new ArrayList<>()
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                char c = grid[y][x]
                if (c >= 'A' && c <= 'Z') {
                    // Horizontal
                    if (x + 1 < width && grid[y][x + 1] >= 'A' && grid[y][x + 1] <= 'Z') {
                        String label = "" + grid[y][x] + grid[y][x + 1]
                        int px = -1
                        if (x > 0 && grid[y][x - 1] == '.') px = x - 1
                        else if (x + 2 < width && grid[y][x + 2] == '.') px = x + 2
                        if (px != -1) foundList.add(new FoundPortal(label, px, y))
                    } else if (y + 1 < height && grid[y + 1][x] >= 'A' && grid[y + 1][x] <= 'Z') {
                        String label = "" + grid[y][x] + grid[y + 1][x]
                        int py = -1
                        if (y > 0 && grid[y - 1][x] == '.') py = y - 1
                        else if (y + 2 < height && grid[y + 2][x] == '.') py = y + 2
                        if (py != -1) foundList.add(new FoundPortal(label, x, py))
                    }
                }
            }
        }

        int startX = 0, startY = 0, endX = 0, endY = 0
        List<FoundPortal> portalsToPair = new ArrayList<>()
        for (FoundPortal fp : foundList) {
            if (fp.label == "AA") { startX = fp.x; startY = fp.y }
            else if (fp.label == "ZZ") { endX = fp.x; endY = fp.y }
            else portalsToPair.add(fp)
        }

        Map<String, List<int[]>> groups = new HashMap<>()
        for (FoundPortal p : portalsToPair) {
            groups.computeIfAbsent(p.label, { k -> new ArrayList<>() })
            groups.get(p.label).add([p.x, p.y] as int[])
        }

        Map<Integer, PortalLink> portalFrom = new HashMap<>()
        for (Map.Entry<String, List<int[]>> en : groups.entrySet()) {
            List<int[]> list = en.getValue()
            if (list.size() == 2) {
                int[] a = list.get(0)
                int[] b = list.get(1)
                int ax = a[0], ay = a[1], bx = b[0], by = b[1]
                boolean aOuter = isOuter(ax, ay, width, height)
                boolean bOuter = isOuter(bx, by, width, height)
                portalFrom.put(ay * width + ax, new PortalLink(bx, by, aOuter))
                portalFrom.put(by * width + bx, new PortalLink(ax, ay, bOuter))
            }
        }

        int total = height * width * MAX_LEVEL
        boolean[] visited = new boolean[total]

        int[] qx = new int[MAX_QUEUE_SIZE]
        int[] qy = new int[MAX_QUEUE_SIZE]
        int[] ql = new int[MAX_QUEUE_SIZE]
        int[] qs = new int[MAX_QUEUE_SIZE]
        int head = 0, tail = 0

        int startIdx = (startY * width + startX) * MAX_LEVEL
        visited[startIdx] = true
        qx[tail] = startX; qy[tail] = startY; ql[tail] = 0; qs[tail] = 0
        tail = (tail + 1) % MAX_QUEUE_SIZE

        int[] dx = [0, 0, 1, -1] as int[]
        int[] dy = [1, -1, 0, 0] as int[]

        while (head != tail) {
            int cx = qx[head]
            int cy = qy[head]
            int cl = ql[head]
            int cs = qs[head]
            head = (head + 1) % MAX_QUEUE_SIZE

            if (cx == endX && cy == endY && cl == 0) {
                println(cs)
                return
            }

            for (int i = 0; i < 4; i++) {
                int nx = cx + dx[i]
                int ny = cy + dy[i]
                if (nx >= 0 && ny >= 0 && nx < width && ny < height && grid[ny][nx] == '.') {
                    int idx = (ny * width + nx) * MAX_LEVEL + cl
                    if (!visited[idx]) {
                        visited[idx] = true
                        qx[tail] = nx
                        qy[tail] = ny
                        ql[tail] = cl
                        qs[tail] = cs + 1
                        tail = (tail + 1) % MAX_QUEUE_SIZE
                        if (tail == head) {
                            println("Queue overflow")
                            return
                        }
                    }
                }
            }

            int k = cy * width + cx
            PortalLink link = portalFrom.get(k)
            if (link != null) {
                int nl = cl + (link.isOuter ? -1 : 1)
                if (nl >= 0 && nl < MAX_LEVEL) {
                    int idx2 = (link.toY * width + link.toX) * MAX_LEVEL + nl
                    if (!visited[idx2]) {
                        visited[idx2] = true
                        qx[tail] = link.toX
                        qy[tail] = link.toY
                        ql[tail] = nl
                        qs[tail] = cs + 1
                        tail = (tail + 1) % MAX_QUEUE_SIZE
                        if (tail == head) {
                            println("Queue overflow")
                            return
                        }
                    }
                }
            }
        }

        println(-1)
    }
}