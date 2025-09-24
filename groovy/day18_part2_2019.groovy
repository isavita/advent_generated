import java.util.concurrent.ArrayBlockingQueue
class Main {
    static class Edge { int to, dist, req }
    static class State { int cost; int[] pos; int mask }
    static void main(String[] args) {
        def lines = new File("input.txt").readLines().findAll{ it.size()>0 }
        int height = lines.size()
        int width = lines[0].length()
        char[][] grid = new char[height][width]
        for (int y=0;y<height;y++) grid[y]=lines[y].toCharArray()
        int sx=-1, sy=-1
        for (int y=0;y<height && sx==-1;y++) for (int x=0;x<width && sx==-1;x++) if (grid[y][x]=='@'){ sx=x; sy=y }
        grid[sy-1][sx-1]='@'; grid[sy-1][sx]='#'; grid[sy-1][sx+1]='@'
        grid[sy][sx-1]='#'; grid[sy][sx]='#'; grid[sy][sx+1]='#'
        grid[sy+1][sx-1]='@'; grid[sy+1][sx]='#'; grid[sy+1][sx+1]='@'
        Map<Character,Integer> keyIndex = [:]
        int nextKey = 0
        List<int[]> nodeCoords = []
        List<int[]> robots = []
        for (int y=0;y<height;y++) for (int x=0;x<width;x++) {
            char c = grid[y][x]
            if (Character.isLowerCase(c)) {
                if (!keyIndex.containsKey(c)) keyIndex[c]=nextKey++
            }
        }
        for (int y=0;y<height;y++) for (int x=0;x<width;x++) {
            char c = grid[y][x]
            if (Character.isLowerCase(c)) nodeCoords.add([x,y] as int[])
            else if (c=='@') robots.add([x,y] as int[])
        }
        if (robots.size()!=4) { println("0"); return }
        int K = nextKey
        int nodeCount = K + 4
        for (int i=0;i<4;i++) nodeCoords.add(robots[i])
        int allMask = (K==0)?0:((1<<K)-1)
        List<List<Edge>> graph = new ArrayList<>(nodeCount)
        for (int i=0;i<nodeCount;i++) graph.add(new ArrayList<Edge>())
        int[] dx = [-1,1,0,0]
        int[] dy = [0,0,-1,1]
        for (int node=0; node<nodeCount; node++) {
            boolean[][] vis = new boolean[height][width]
            int[] start = nodeCoords[node]
            int qcap = width*height
            int[] qx = new int[qcap], qy = new int[qcap], qd = new int[qcap], qr = new int[qcap]
            int head=0, tail=0
            qx[tail]=start[0]; qy[tail]=start[1]; qd[tail]=0; qr[tail]=0; tail++
            vis[start[1]][start[0]]=true
            while (head<tail) {
                int x=qx[head], y=qy[head], dist=qd[head], req=qr[head]; head++
                char cell = grid[y][x]
                if (Character.isLowerCase(cell)) {
                    int id = keyIndex[cell]
                    if (id>=0 && id!=node) graph.get(node).add(new Edge(to:id, dist:dist, req:req))
                }
                for (int k=0;k<4;k++) {
                    int nx=x+dx[k], ny=y+dy[k]
                    if (nx<0||nx>=width||ny<0||ny>=height) continue
                    if (vis[ny][nx]) continue
                    char nc = grid[ny][nx]
                    if (nc=='#') continue
                    int nreq = req
                    if (Character.isUpperCase(nc)) {
                        char lk = Character.toLowerCase(nc)
                        if (keyIndex.containsKey(lk)) nreq |= 1<<keyIndex[lk]
                        else { vis[ny][nx]=true; continue }
                    }
                    vis[ny][nx]=true
                    qx[tail]=nx; qy[tail]=ny; qd[tail]=dist+1; qr[tail]=nreq; tail++
                }
            }
        }
        int bits=1
        while ((1<<bits) < nodeCount) bits++
        long maskShift = bits*4
        def pack = { int[] pos, int mask ->
            long p = (((((pos[0]<<bits)|pos[1])<<bits)|pos[2])<<bits)|pos[3]
            return (((long)mask)<<maskShift) | (p & ((1L<<(bits*4))-1L))
        }
        PriorityQueue<State> pq = new PriorityQueue<>({ a,b -> a.cost - b.cost } as Comparator)
        State init = new State()
        init.cost=0
        init.pos = new int[4]
        for (int i=0;i<4;i++) init.pos[i]=K+i
        init.mask=0
        Map<Long,Integer> best = new HashMap<>()
        long pk = pack(init.pos, init.mask)
        best.put(pk,0)
        pq.add(init)
        int result = -1
        while (!pq.isEmpty()) {
            State s = pq.poll()
            long key = pack(s.pos, s.mask)
            if (best.getOrDefault(key, Integer.MAX_VALUE) != s.cost) continue
            if (s.mask == allMask) { result = s.cost; break }
            for (int r=0;r<4;r++) {
                int from = s.pos[r]
                for (Edge e : graph.get(from)) {
                    int tgt = e.to
                    if (tgt<0 || tgt>=K) continue
                    int bit = 1<<tgt
                    if ((s.mask & bit) != 0) continue
                    if ((e.req & ~s.mask) != 0) continue
                    int nmask = s.mask | bit
                    int ncost = s.cost + e.dist
                    int[] npos = s.pos.clone()
                    npos[r]=tgt
                    long nkey = pack(npos, nmask)
                    if (ncost < best.getOrDefault(nkey, Integer.MAX_VALUE)) {
                        best.put(nkey, ncost)
                        State ns = new State()
                        ns.cost=ncost; ns.pos=npos; ns.mask=nmask
                        pq.add(ns)
                    }
                }
            }
        }
        println(result)
    }
}
Main.main(null)