
class Main {
    static int countPaths(String cur, String target, Map adj, Map memo) {
        if (cur == target) return 1
        if (memo.containsKey(cur)) return memo[cur]
        int total = 0
        adj.get(cur, []).each { neighbor ->
            total += countPaths(neighbor, target, adj, memo)
        }
        memo[cur] = total
        total
    }

    static void main(String[] args) {
        def adj = [:].withDefault { [] }
        new File('input.txt').eachLine { line ->
            line = line.trim()
            if (!line) return
            def parts = line.split(':')
            if (parts.size() != 2) return
            def source = parts[0].trim()
            def dests = parts[1].trim().split(/\s+/)
            adj[source] = dests as List
        }
        def memo = [:]
        int count = countPaths('you', 'out', adj, memo)
        println "Number of paths from 'you' to 'out': $count"
    }
}
