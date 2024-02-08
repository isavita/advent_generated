def input = new File("input.txt").readLines()

def surfaceArea = 0

input.each {
    def coords = it.split(",").collect { it as int }
    
    def x = coords[0]
    def y = coords[1]
    def z = coords[2]
    
    def neighbors = [[x+1, y, z], [x-1, y, z], [x, y+1, z], [x, y-1, z], [x, y, z+1], [x, y, z-1]]
    
    def sides = neighbors.count { neighbor ->
        !input.contains(neighbor.join(","))
    }
    
    surfaceArea += sides
}

println surfaceArea