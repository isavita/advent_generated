
class LANParty {

    static void main(String[] args) {
        def connections = readConnections("input.txt")
        def adjList = buildAdjacencyList(connections)

        // Part 1: Find sets of three interconnected computers with at least one starting with 't'
        int countPart1 = countTrianglesWithT(adjList)
        println "Part 1: Number of sets with at least one 't': ${countPart1}"

        // Part 2: Find the largest fully connected component (clique)
        def largestClique = findLargestClique(adjList)
        def password = largestClique.sort().join(',')
        println "Part 2: Password for the LAN party: ${password}"
    }

    static List<String> readConnections(String filename) {
        new File(filename).readLines()
    }

    static Map<String, Set<String>> buildAdjacencyList(List<String> connections) {
        def adjList = [:]
        connections.each { connection ->
            def (node1, node2) = connection.split('-')
            adjList.computeIfAbsent(node1, { new HashSet<String>() }).add(node2)
            adjList.computeIfAbsent(node2, { new HashSet<String>() }).add(node1) // Bidirectional
        }
        return adjList
    }
    
    static int countTrianglesWithT(Map<String, Set<String>> adjList) {
         adjList.keySet().sum { node1 ->
            if (!node1.startsWith('t')) {
                 adjList[node1].sum { node2 ->
                      adjList[node2].count { node3 ->
                        node1 < node2 && node2 < node3 && adjList[node1].contains(node3)
                    }
                }
            }else{
                adjList[node1].sum { node2 ->
                  adjList[node2].count { node3 ->
                     node1 < node2 && node2 < node3 && adjList[node1].contains(node3)
                  }
              }
            }
        }
    }


    static Set<String> findLargestClique(Map<String, Set<String>> adjList) {
      def nodes = adjList.keySet() as List
      def largestClique = [] as Set

      // Iterate through all possible subsets (Bron-Kerbosch Algorithm - optimized)
      findCliques(adjList, [] as Set, nodes, [] as Set, largestClique)

      return largestClique
    }

    static void findCliques(Map<String, Set<String>> adjList, Set<String> R, List<String> P, Set<String> X, Set<String> largestClique) {
      if (P.isEmpty() && X.isEmpty()) {
        if (R.size() > largestClique.size()) {
            largestClique.clear()
            largestClique.addAll(R)
        }
        return
      }

      // Choose a pivot node (optimization)
        def pivot = P.isEmpty() ? X.first() : P.first()

        def pWithoutNeighborsOfPivot = P.findAll { !adjList[pivot].contains(it) }

      pWithoutNeighborsOfPivot.each { v ->
        def newR = R.clone() as Set
        newR.add(v)

        def newP = P.findAll { adjList[v].contains(it) }
        def newX = X.findAll { adjList[v].contains(it) }

        findCliques(adjList, newR, newP, newX, largestClique)

        P.remove(v)
        X.add(v)
      }
    }
}
