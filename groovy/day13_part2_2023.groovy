
class Day13 {

    static int solve(List<List<String>> pattern) {
        int cols = findReflection(pattern, -1)
        if (cols != -1) {
            return cols
        }
        
        List<List<String>> transposed = transpose(pattern)
        int rows = findReflection(transposed, -1)
        if(rows != -1){
            return rows * 100
        }
        return 0
    }

    static int solvePart2(List<List<String>> pattern) {
        int originalCols = findReflection(pattern, -1);
        int originalRows = findReflection(transpose(pattern), -1);

        for (int y = 0; y < pattern.size(); y++) {
            for (int x = 0; x < pattern[0].size(); x++) {
                List<List<String>> copy = deepCopy(pattern);
                copy[y][x] = (copy[y][x] == "#") ? "." : "#";

                int newCols = findReflection(copy, originalCols);
                if (newCols != -1 && newCols != originalCols) {
                    return newCols;
                }

                List<List<String>> transposedCopy = transpose(copy);
                int newRows = findReflection(transposedCopy, originalRows);
                if (newRows != -1 && newRows != originalRows) {
                    return newRows * 100;
                }
            }
        }
        return 0;
    }


    static int findReflection(List<List<String>> pattern, int exclude) {
        for (int i = 1; i < pattern[0].size(); i++) {
            if(i == exclude){
                continue
            }
            boolean isReflection = true;
            for (int j = 0; j < pattern.size(); j++) {
                for (int k = 0; k < Math.min(i, pattern[0].size() - i); k++) {
                    if (i - 1 - k < 0 || i + k >= pattern[0].size()) {
                        continue;
                    }
                    if (pattern[j].get(i - 1 - k) != pattern[j].get(i + k)) {
                        isReflection = false;
                        break;
                    }
                }
                if (!isReflection) {
                    break;
                }
            }
            if (isReflection) {
                return i;
            }
        }
        return -1;
    }


    static List<List<String>> transpose(List<List<String>> matrix) {
        int rows = matrix.size();
        int cols = matrix[0].size();
        List<List<String>> transposed = new ArrayList<>();
        for (int i = 0; i < cols; i++) {
            transposed.add(new ArrayList<>());
        }

        for (int i = 0; i < cols; i++) {
            for (int j = 0; j < rows; j++) {
                transposed[i].add(matrix[j].get(i));
            }
        }
        return transposed;
    }
    
    static List<List<String>> deepCopy(List<List<String>> original) {
        List<List<String>> copy = new ArrayList<>();
        for (List<String> row : original) {
            copy.add(new ArrayList<>(row));
        }
        return copy;
    }

    static void main(String[] args) {
        File inputFile = new File("input.txt")
        List<List<List<String>>> patterns = []
        List<String> currentPattern = []

        inputFile.eachLine { line ->
            if (line.trim().isEmpty()) {
                if (!currentPattern.isEmpty()) {
                    patterns.add(currentPattern.collect { it.toList() })
                }
                currentPattern = []
            } else {
                currentPattern.add(line)
            }
        }
        if (!currentPattern.isEmpty()) {
            patterns.add(currentPattern.collect { it.toList() })
        }

        long total = 0
        patterns.each { pattern ->
            total += solve(pattern)
        }
        println "Part 1: ${total}"

        long total2 = 0
        patterns.each { pattern ->
            total2 += solvePart2(pattern)
        }
        println "Part 2: ${total2}"
    }
}
