
import java.nio.file.Files
import java.nio.file.Paths

class UF{
    int[] p, sz
    UF(int n){
        p = new int[n]
        sz = new int[n]
        for(i in 0..<n){p[i]=i;sz[i]=1}
    }
    int find(int x){
        while(p[x]!=x){
            p[x]=p[p[x]]
            x=p[x]
        }
        return x
    }
    void union(int a,int b){
        int ra=find(a), rb=find(b)
        if(ra==rb) return
        if(sz[ra]<sz[rb]){int t=ra;ra=rb;rb=t}
        p[rb]=ra
        sz[ra]+=sz[rb]
    }
}

def lines = Files.readAllLines(Paths.get('input.txt'))
def pts = []
lines.each{
    def m = it =~ /(\-?\d+)\s*,\s*(\-?\d+)\s*,\s*(\-?\d+)/
    if(m.matches()){
        pts << [m[0][1] as int, m[0][2] as int, m[0][3] as int]
    }
}
int n = pts.size()
if(n<2){
    println "Not enough points to form circuits."
    return
}
def edges = []
for(i in 0..<n){
    for(j in i+1..<n){
        long dx=pts[i][0]-pts[j][0]
        long dy=pts[i][1]-pts[j][1]
        long dz=pts[i][2]-pts[j][2]
        edges << [i,j,dx*dx+dy*dy+dz*dz]
    }
}
edges.sort{a,b->a[2]<=>b[2]}
def uf = new UF(n)
int limit = Math.min(edges.size(),1000)
for(k in 0..<limit){
    uf.union(edges[k][0],edges[k][1])
}
def top = [0,0,0]
for(i in 0..<n){
    if(uf.p[i]==i){
        int s=uf.sz[i]
        if(s>top[0]){
            top[2]=top[1];top[1]=top[0];top[0]=s
        }else if(s>top[1]){
            top[2]=top[1];top[1]=s
        }else if(s>top[2]){
            top[2]=s
        }
    }
}
def result = 1L
top.each{if(it>0) result*=it}
println "Product of three largest circuit sizes: $result"
