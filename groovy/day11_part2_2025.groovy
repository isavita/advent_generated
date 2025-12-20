
class Main {
    static Map<String,Integer> id = [:]
    static List<List<Integer>> g = []
    static int idx(String s){
        id.computeIfAbsent(s){ int i=id.size(); g << []; i }
    }
    static void addEdge(int u,int v){
        g[u] << v
    }
    static long dfs(int cur,int tgt,long[] memo){
        if(cur==tgt) return 1
        if(memo[cur]!=-1) return memo[cur]
        long sum=0
        g[cur].each{ sum+=dfs(it,tgt,memo) }
        memo[cur]=sum
        sum
    }
    static long count(int s,int t){
        long[] memo = new long[g.size()]
        java.util.Arrays.fill(memo,-1L)
        dfs(s,t,memo)
    }
    static void main(String[] args){
        new File('input.txt').eachLine{ line->
            line=line.trim()
            if(!line) return
            def parts=line.split(':',2)
            if(parts.size()!=2) return
            int u=idx(parts[0].trim())
            parts[1].trim().split(/\s+/).each{ v-> addEdge(u, idx(v)) }
        }
        int svr=idx('svr'), dac=idx('dac'), fft=idx('fft'), out=idx('out')
        long s1=count(svr,dac)*count(dac,fft)*count(fft,out)
        long s2=count(svr,fft)*count(fft,dac)*count(dac,out)
        println "Paths (svr->dac->fft->out): $s1"
        println "Paths (svr->fft->dac->out): $s2"
        println "Total paths visiting both: ${s1+s2}"
    }
}
