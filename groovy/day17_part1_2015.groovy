def containers = []
new File('input.txt').eachLine { line -> containers.add(line.toInteger()) }

int target = 150
int count = 0

for (int i = 1; i < (1 << containers.size()); i++) {
    int sum = 0
    for (int j = 0; j < containers.size(); j++) {
        if ((i & (1 << j)) != 0) {
            sum += containers[j]
        }
    }
    if (sum == target) {
        count++
    }
}

println count