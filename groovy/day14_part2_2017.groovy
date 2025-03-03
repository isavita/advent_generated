
class Day14 {

    static String knotHash(String input) {
        List<Integer> lengths = input.bytes.collect { it as Integer }
        lengths.addAll([17, 31, 73, 47, 23])

        List<Integer> list = (0..<256).collect()
        int currentPosition = 0
        int skipSize = 0

        64.times {
            lengths.each { length ->
                List<Integer> sublist = []
                for (int i = 0; i < length; i++) {
                    sublist.add(list[(currentPosition + i) % list.size()])
                }
                sublist = sublist.reverse()
                for (int i = 0; i < length; i++) {
                    list[(currentPosition + i) % list.size()] = sublist[i]
                }
                currentPosition = (currentPosition + length + skipSize) % list.size()
                skipSize++
            }
        }

        List<Integer> denseHash = []
        for (int i = 0; i < 16; i++) {
            int xor = 0
            for (int j = 0; j < 16; j++) {
                xor ^= list[i * 16 + j]
            }
            denseHash.add(xor)
        }

        return denseHash.collect { String.format("%02x", it) }.join()
    }

    static int hexToBinary(String hex) {
        int count = 0;
        for(char c: hex.toCharArray()){
            int i = Integer.parseInt(c.toString(), 16);
            String bin = Integer.toBinaryString(i)
            bin = String.format("%4s", bin).replace(' ', '0')
            for(char b : bin.toCharArray()){
                if(b=='1') count++
            }
        }
        return count
    }
    
    static void dfs(int[][] grid, int r, int c) {
        if (r < 0 || r >= 128 || c < 0 || c >= 128 || grid[r][c] == 0) {
            return
        }
        grid[r][c] = 0 // Mark as visited
        dfs(grid, r + 1, c)
        dfs(grid, r - 1, c)
        dfs(grid, r, c + 1)
        dfs(grid, r, c - 1)
    }

    static int countRegions(int[][] grid) {
        int regions = 0
        for (int r = 0; r < 128; r++) {
            for (int c = 0; c < 128; c++) {
                if (grid[r][c] == 1) {
                    regions++
                    dfs(grid, r, c)
                }
            }
        }
        return regions
    }


    static void main(String[] args) {
        String key = new File("input.txt").text.trim()

        // Part 1
        int usedSquares = 0
        int[][] grid = new int[128][128]

        for (int i = 0; i < 128; i++) {
             String hash = knotHash(key + "-" + i)
            usedSquares += hexToBinary(hash)
            
            int col = 0
            for(char h : hash.toCharArray()){
                int intVal = Integer.parseInt(h.toString(), 16)
                String binaryString = String.format("%4s", Integer.toBinaryString(intVal)).replace(' ', '0')
                for(char bit : binaryString.toCharArray()){
                  grid[i][col++] = Integer.parseInt(bit.toString())
                }            
            }
        }
        
        println "Part 1: $usedSquares"


        // Part 2
        
        int regions = countRegions(grid)
        println "Part 2: $regions"
    }
}
