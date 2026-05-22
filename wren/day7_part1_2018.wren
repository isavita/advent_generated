
import "io" for File

var content = File.read("input.txt")
var indegree = List.filled(26, 0)
var graph = (0...26).map { List.filled(26, 0) }.toList
var steps = List.filled(26, false)

for (line in content.split("\n")) {
    if (line.bytes.count >= 37) {
        var i = line[5].codePoints[0] - 65
        var j = line[36].codePoints[0] - 65
        if (graph[i][j] == 0) {
            graph[i][j] = 1
            indegree[j] = indegree[j] + 1
        }
        steps[i] = true
        steps[j] = true
    }
}

var totalSteps = 0
for (i in 0...26) {
    if (steps[i]) {
        totalSteps = totalSteps + 1
    } else {
        indegree[i] = -1
    }
}

var order = ""
var completed = 0
while (completed < totalSteps) {
    for (i in 0...26) {
        if (indegree[i] == 0) {
            order = order + String.fromCodePoint(i + 65)
            indegree[i] = -1
            completed = completed + 1
            for (j in 0...26) {
                if (graph[i][j] == 1) {
                    indegree[j] = indegree[j] - 1
                }
            }
            break
        }
    }
}

System.print(order)
