import java.util.*
class Main {
    static long energyFor(char c) {
        switch(c) {
            case 'A': return 1L
            case 'B': return 10L
            case 'C': return 100L
            case 'D': return 1000L
        }
        return 0L
    }
    static long calcEnergy(char amph, int sr, int sc, int er, int ec) {
        long dist = Math.abs(ec - sc) + (sr - 1) + (er - 1)
        return energyFor(amph) * dist
    }
    static boolean isRoomEntrance(int r, int c, int rows) {
        return r==1 && (c==3 || c==5 || c==7 || c==9)
    }
    static boolean isRoomCell(int r, int c, int rows) {
        return (c==3 || c==5 || c==7 || c==9) && r>=2 && r<=rows-2
    }
    static char wantFor(int r, int c) {
        if (c==3) return 'A'
        if (c==5) return 'B'
        if (c==7) return 'C'
        if (c==9) return 'D'
        return 0 as char
    }
    static List<int[]> getUnsettled(char[][] grid) {
        int rows = grid.length
        int cols = grid[0].length
        List<int[]> res = new ArrayList<>()
        for (int col=1; col<cols; col++) {
            char ch = grid[1][col]
            if (ch>='A' && ch<='D') res.add(new int[]{1,col})
        }
        int[] roomCols = [3,5,7,9]
        for (int col : roomCols) {
            boolean roomFullFromBack = true
            for (int r = rows-2; r>1; r--) {
                char cur = grid[r][col]
                if (cur != '.') {
                    char desired = wantFor(r,col)
                    if (desired==0 || cur != desired) {
                        roomFullFromBack = false
                        res.add(new int[]{r,col})
                    } else if (cur == desired && !roomFullFromBack) {
                        res.add(new int[]{r,col})
                    }
                } else {
                    roomFullFromBack = false
                }
            }
        }
        return res
    }
    static List<int[]> getNextMoves(char[][] grid, int sr, int sc) {
        int rows = grid.length
        int cols = grid[0].length
        char amph = grid[sr][sc]
        boolean startedInHall = (sr==1)
        boolean[][] visited = new boolean[rows][cols]
        ArrayDeque<int[]> q = new ArrayDeque<>()
        List<int[]> dests = new ArrayList<>()
        visited[sr][sc]=true
        q.add(new int[]{sr,sc})
        int[] dr = [-1,1,0,0]
        int[] dc = [0,0,-1,1]
        while(!q.isEmpty()) {
            int[] cur = q.removeFirst()
            for (int i=0;i<4;i++) {
                int nr = cur[0]+dr[i], nc = cur[1]+dc[i]
                if (nr>=0 && nr<rows && nc>=0 && nc<cols && !visited[nr][nc] && grid[nr][nc]=='.') {
                    visited[nr][nc]=true
                    q.add(new int[]{nr,nc})
                    boolean nextIsHall = (nr==1)
                    boolean nextIsEntrance = nextIsHall && (nc==3 || nc==5 || nc==7 || nc==9)
                    if (nextIsEntrance) continue
                    if (startedInHall) {
                        if (!nextIsHall) {
                            char want = wantFor(nr,nc)
                            if (want==amph) {
                                boolean hasDeeperOpen=false
                                boolean hasWrong=false
                                for (int r=nr+1;r<rows-1;r++) {
                                    char cc = grid[r][nc]
                                    if (cc=='.') { hasDeeperOpen=true; break }
                                    if (cc!=amph) { hasWrong=true; break }
                                }
                                if (!hasDeeperOpen && !hasWrong) dests.add(new int[]{nr,nc})
                            }
                        }
                    } else {
                        if (nextIsHall) dests.add(new int[]{nr,nc})
                    }
                }
            }
        }
        return dests
    }
    static String serialize(char[][] g) {
        StringBuilder sb = new StringBuilder()
        for (int r=0;r<g.length;r++) {
            sb.append(new String(g[r]))
            sb.append('\n')
        }
        return sb.toString()
    }
    static void main(String[] args) {
        List<String> lines = new ArrayList<>()
        new File("input.txt").eachLine { lines.add(it) }
        int origRows = lines.size()
        int cols = lines[0].length()
        char[][] init = new char[origRows][cols]
        for (int r=0;r<origRows;r++) init[r] = lines[r].toCharArray()
        int partRows = origRows + 2
        char[][] grid = new char[partRows][cols]
        grid[0] = init[0].clone()
        grid[1] = init[1].clone()
        grid[partRows-1] = init[origRows-1].clone()
        grid[2] = init[2].clone()
        grid[5] = init[3].clone()
        grid[6] = init[4].clone()
        grid[3] = "  #D#C#B#A#  ".toCharArray()
        grid[4] = "  #D#B#A#C#  ".toCharArray()
        long minEnergy = Long.MAX_VALUE
        Comparator<Object> cmp = { a,b ->
            long ea = ((Object[])a)[1] as long
            long eb = ((Object[])b)[1] as long
            return ea<eb ? -1 : (ea>eb ? 1 : 0)
        } as Comparator
        PriorityQueue pq = new PriorityQueue(cmp)
        pq.add([grid,0L])
        Set<String> seen = new HashSet<>()
        while(!pq.isEmpty()) {
            def entry = pq.poll()
            char[][] curGrid = entry[0] as char[][]
            long curE = entry[1] as long
            if (curE >= minEnergy) continue
            boolean done = true
            for (int r=2;r<=partRows-2 && done;r++) {
                for (int c : [3,5,7,9]) {
                    if (curGrid[r][c] != wantFor(r,c)) { done = false; break }
                }
            }
            if (done) { if (curE < minEnergy) minEnergy = curE; continue }
            String sig = serialize(curGrid)
            if (seen.contains(sig)) continue
            seen.add(sig)
            List<int[]> unsettled = getUnsettled(curGrid)
            for (int[] pos : unsettled) {
                int sr = pos[0], sc = pos[1]
                List<int[]> moves = getNextMoves(curGrid,sr,sc)
                char amph = curGrid[sr][sc]
                for (int[] mv : moves) {
                    int er = mv[0], ec = mv[1]
                    long cost = calcEnergy(amph,sr,sc,er,ec)
                    long newE = curE + cost
                    if (newE >= minEnergy) continue
                    char[][] ng = new char[partRows][cols]
                    for (int r=0;r<partRows;r++) ng[r] = curGrid[r].clone()
                    ng[er][ec] = amph
                    ng[sr][sc] = '.'
                    pq.add([ng,newE])
                }
            }
        }
        println minEnergy
    }
}