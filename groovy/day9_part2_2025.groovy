
import java.util.*
import java.nio.file.*

class Point {
    int x, y
    Point(int x, int y){ this.x = x; this.y = y }
}

static int abs(int a){ a < 0 ? -a : a }

static void main(String[] args) {
    def points = []
    def uniqX = new HashSet<Integer>()
    def uniqY = new HashSet<Integer>()
    new File('input.txt').eachLine { line ->
        line = line.trim()
        if (!line) return
        def parts = line.split(',')
        if (parts.size() != 2) return
        int x = parts[0] as int
        int y = parts[1] as int
        points << new Point(x, y)
        uniqX << x
        uniqY << y
    }
    if (!points) { println 'No points found.'; return }

    def uniqXList = uniqX as List
    def uniqYList = uniqY as List
    Collections.sort(uniqXList)
    Collections.sort(uniqYList)

    def xIdx = [:]
    uniqXList.eachWithIndex { v,i -> xIdx[v]=i }
    def yIdx = [:]
    uniqYList.eachWithIndex { v,i -> yIdx[v]=i }

    int w = 2*uniqXList.size()+1
    int h = 2*uniqYList.size()+1
    long[] colW = new long[w]
    long[] rowH = new long[h]

    colW[0]=1
    uniqXList.eachWithIndex { v,i ->
        colW[2*i+1]=1
        if (i<uniqXList.size()-1){
            int gap = uniqXList[i+1]-v-1
            colW[2*i+2]=gap>0?gap:0
        } else colW[2*i+2]=1
    }
    rowH[0]=1
    uniqYList.eachWithIndex { v,i ->
        rowH[2*i+1]=1
        if (i<uniqYList.size()-1){
            int gap = uniqYList[i+1]-v-1
            rowH[2*i+2]=gap>0?gap:0
        } else rowH[2*i+2]=1
    }

    byte[][] grid = new byte[h][w]

    Closure toGrid = { Point p ->
        int gx = 2*xIdx[p.x]+1
        int gy = 2*yIdx[p.y]+1
        [gx,gy]
    }

    int n = points.size()
    for (int i=0;i<n;i++){
        def p1=points[i]
        def p2=points[(i+1)%n]
        def (gx1,gy1)=toGrid(p1)
        def (gx2,gy2)=toGrid(p2)
        if (gx1==gx2){
            int s=gy1, e=gy2
            if (s>e){ s=gy2; e=gy1 }
            for (int y=s;y<=e;y++) if (rowH[y]>0) grid[y][gx1]=1
        }else{
            int s=gx1, e=gx2
            if (s>e){ s=gx2; e=gx1 }
            for (int x=s;x<=e;x++) if (colW[x]>0) grid[gy1][x]=1
        }
    }

    Deque<Point> q = new ArrayDeque<>()
    q.add(new Point(0,0))
    grid[0][0]=2
    int[][] dirs = [[0,1],[0,-1],[1,0],[-1,0]]
    while(!q.isEmpty()){
        def cur = q.poll()
        dirs.each{ d ->
            int nx=cur.x+d[0], ny=cur.y+d[1]
            if (nx>=0 && nx<w && ny>=0 && ny<h && grid[ny][nx]==0){
                grid[ny][nx]=2
                q.add(new Point(nx,ny))
            }
        }
    }

    long[][] pref = new long[h][w]
    for (int y=0;y<h;y++){
        for (int x=0;x<w;x++){
            long val = grid[y][x]==2?0:colW[x]*rowH[y]
            long left = x>0?pref[y][x-1]:0
            long up   = y>0?pref[y-1][x]:0
            long diag = (x>0 && y>0)?pref[y-1][x-1]:0
            pref[y][x]=val+left+up-diag
        }
    }

    Closure sum = { int x1,int y1,int x2,int y2 ->
        if (x1>x2){ int t=x1;x1=x2;x2=t }
        if (y1>y2){ int t=y1;y1=y2;y2=t }
        long total = pref[y2][x2]
        long left = x1>0?pref[y2][x1-1]:0
        long up   = y1>0?pref[y1-1][x2]:0
        long diag = (x1>0 && y1>0)?pref[y1-1][x1-1]:0
        total-left-up+diag
    }

    long maxArea=0
    for (int i=0;i<n;i++){
        for (int j=i;j<n;j++){
            def p1=points[i], p2=points[j]
            long realW = abs(p1.x-p2.x)+1L
            long realH = abs(p1.y-p2.y)+1L
            long area = realW*realH
            if (area<=maxArea) continue
            def (gx1,gy1)=toGrid(p1)
            def (gx2,gy2)=toGrid(p2)
            if (sum(gx1,gy1,gx2,gy2)==area) maxArea=area
        }
    }
    println "Largest valid area: $maxArea"
}
