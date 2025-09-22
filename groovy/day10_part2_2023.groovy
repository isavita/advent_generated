import java.io.File
class PipeSolver {
    static final String EMPTY = "."
    static final String START = "S"
    static final String VERTICAL = "|"
    static final String HORIZONTAL = "-"
    static final String TOP_LEFT_CORNER = "J"
    static final String TOP_RIGHT_CORNER = "L"
    static final String BOTTOM_LEFT_CORNER = "7"
    static final String BOTTOM_RIGHT_CORNER = "F"

    static final Pipe VERTICAL_PIPE = new Pipe(true,false,true,false)
    static final Pipe HORIZONTAL_PIPE = new Pipe(false,true,false,true)
    static final Pipe TOP_LEFT_CORNER_PIPE = new Pipe(true,false,false,true)
    static final Pipe TOP_RIGHT_CORNER_PIPE = new Pipe(true,true,false,false)
    static final Pipe BOTTOM_LEFT_CORNER_PIPE = new Pipe(false,false,true,true)
    static final Pipe BOTTOM_RIGHT_CORNER_PIPE = new Pipe(false,true,true,false)

    static final Coord TOP = new Coord(0,-1)
    static final Coord RIGHT = new Coord(1,0)
    static final Coord BOTTOM = new Coord(0,1)
    static final Coord LEFT = new Coord(-1,0)

    static Coord coordAdd(Coord a, Coord b){ new Coord(a.x+b.x, a.y+b.y) }
    static Coord coordOpposite(Coord c){ new Coord(-c.x, -c.y) }
    static boolean pipeEquals(Pipe p1, Pipe p2){
        if(p1==null||p2==null) return false
        return p1.top==p2.top && p1.right==p2.right && p1.bottom==p2.bottom && p1.left==p2.left
    }
    static Pipe getPipeFromTile(String tile){
        switch(tile){
            case VERTICAL: return VERTICAL_PIPE
            case HORIZONTAL: return HORIZONTAL_PIPE
            case TOP_LEFT_CORNER: return TOP_LEFT_CORNER_PIPE
            case TOP_RIGHT_CORNER: return TOP_RIGHT_CORNER_PIPE
            case BOTTOM_LEFT_CORNER: return BOTTOM_LEFT_CORNER_PIPE
            case BOTTOM_RIGHT_CORNER: return BOTTOM_RIGHT_CORNER_PIPE
            default: return new Pipe(false,false,false,false)
        }
    }
    static String getTileFromPipe(Pipe pipe){
        if(pipeEquals(pipe, VERTICAL_PIPE)) return VERTICAL
        if(pipeEquals(pipe, HORIZONTAL_PIPE)) return HORIZONTAL
        if(pipeEquals(pipe, TOP_LEFT_CORNER_PIPE)) return TOP_LEFT_CORNER
        if(pipeEquals(pipe, TOP_RIGHT_CORNER_PIPE)) return TOP_RIGHT_CORNER
        if(pipeEquals(pipe, BOTTOM_LEFT_CORNER_PIPE)) return BOTTOM_LEFT_CORNER
        if(pipeEquals(pipe, BOTTOM_RIGHT_CORNER_PIPE)) return BOTTOM_RIGHT_CORNER
        return EMPTY
    }
    static Pipe getPipeFromNeighbors(Coord c, Grid grid){
        Pipe pipe = new Pipe(false,false,false,false)
        Coord[] dirs = [TOP, RIGHT, BOTTOM, LEFT]
        Coord[] neigh = [
            coordAdd(c, TOP),
            coordAdd(c, RIGHT),
            coordAdd(c, BOTTOM),
            coordAdd(c, LEFT)
        ]
        for(int i=0;i<4;i++){
            Coord nc = neigh[i]
            if(nc.x>=0 && nc.x<grid.width && nc.y>=0 && nc.y<grid.height){
                String neighborTile = grid.data[nc.y*grid.width + nc.x]
                Pipe neighborPipe = getPipeFromTile(neighborTile)
                Coord opp = coordOpposite(dirs[i])
                boolean connected = (opp.x==0 && opp.y==-1 && neighborPipe.top) ||
                                   (opp.x==1 && opp.y==0 && neighborPipe.right) ||
                                   (opp.x==0 && opp.y==1 && neighborPipe.bottom) ||
                                   (opp.x==-1 && opp.y==0 && neighborPipe.left)
                if(connected){
                    if(dirs[i].x==0 && dirs[i].y==-1) pipe.top = true
                    if(dirs[i].x==1 && dirs[i].y==0) pipe.right = true
                    if(dirs[i].x==0 && dirs[i].y==1) pipe.bottom = true
                    if(dirs[i].x==-1 && dirs[i].y==0) pipe.left = true
                }
            }
        }
        return pipe
    }

    static List<Coord> pathFinding(Coord start, Grid grid){
        List<Coord> path = new ArrayList<>()
        path.add(start)
        Pipe startPipe = getPipeFromNeighbors(start, grid)
        Coord previousDir
        Coord current
        if(startPipe.top){ previousDir = TOP; current = coordAdd(start, TOP) }
        else if(startPipe.right){ previousDir = RIGHT; current = coordAdd(start, RIGHT) }
        else if(startPipe.bottom){ previousDir = BOTTOM; current = coordAdd(start, BOTTOM) }
        else { previousDir = LEFT; current = coordAdd(start, LEFT) }

        while(!(current.x==start.x && current.y==start.y)){
            path.add(current)
            String tile = grid.data[current.y*grid.width + current.x]
            Pipe currentPipe = getPipeFromTile(tile)
            if(currentPipe.top && (previousDir.x != 0 || previousDir.y != 1)){
                previousDir = TOP
                current = coordAdd(current, TOP)
            } else if(currentPipe.right && (previousDir.x != -1 || previousDir.y != 0)){
                previousDir = RIGHT
                current = coordAdd(current, RIGHT)
            } else if(currentPipe.bottom && (previousDir.x != 0 || previousDir.y != -1)){
                previousDir = BOTTOM
                current = coordAdd(current, BOTTOM)
            } else {
                previousDir = LEFT
                current = coordAdd(current, LEFT)
            }
        }
        return path
    }

    static Grid getPathGrid(Grid grid, List<Coord> path){
        Grid newGrid = new Grid(grid.width, grid.height)
        for(int i=0;i<path.size();i++){
            Coord c = path[i]
            newGrid.data[c.y*grid.width + c.x] = grid.data[c.y*grid.width + c.x]
        }
        Coord start = path[0]
        newGrid.data[start.y*grid.width + start.x] = getTileFromPipe(getPipeFromNeighbors(start, grid))
        return newGrid
    }

    static boolean isInside(Coord c, Grid grid){
        if(c.x<0 || c.y<0 || c.x>=grid.width || c.y>=grid.height) return false
        if(grid.data[c.y*grid.width + c.x] != EMPTY) return false

        String startPipe = EMPTY
        int numPipeOnLeft = 0
        for(int x=0; x<c.x; x++){
            Coord coord = new Coord(x, c.y)
            String v = grid.data[coord.y*grid.width + coord.x]
            String sv = v ?: EMPTY
            if(sv == VERTICAL){
                numPipeOnLeft++
            } else if(sv == TOP_RIGHT_CORNER){
                startPipe = TOP_RIGHT_CORNER
            } else if(sv == BOTTOM_RIGHT_CORNER){
                startPipe = BOTTOM_RIGHT_CORNER
            } else if(sv == TOP_LEFT_CORNER){
                if(startPipe == BOTTOM_RIGHT_CORNER){
                    startPipe = EMPTY
                    numPipeOnLeft++
                } else if(sv == TOP_RIGHT_CORNER){
                    startPipe = EMPTY
                }
            } else if(sv == BOTTOM_LEFT_CORNER){
                if(startPipe == TOP_RIGHT_CORNER){
                    startPipe = EMPTY
                    numPipeOnLeft++
                } else if(startPipe == BOTTOM_RIGHT_CORNER){
                    startPipe = EMPTY
                }
            }
        }
        return (numPipeOnLeft % 2) == 1
    }

    static int solve(List<String> input){
        int height = input.size()
        if(height==0) return 0
        int width = input[0].length()
        Grid grid = buildGrid(input, height, width)
        Coord start = findStart(grid)
        if(start == null) return 0
        List<Coord> path = pathFinding(start, grid)
        Grid pathGrid = getPathGrid(grid, path)
        int cnt = 0
        for(int y=0; y<pathGrid.height; y++){
            for(int x=0; x<pathGrid.width; x++){
                if(isInside(new Coord(x,y), pathGrid)){
                    cnt++
                }
            }
        }
        return cnt
    }

    static Grid buildGrid(List<String> input, int height, int width){
        Grid grid = new Grid(width, height)
        for(int y=0;y<height;y++){
            String line = input[y]
            for(int x=0;x<width;x++){
                String ch = line.charAt(x).toString()
                if(ch != EMPTY){
                    grid.data[y*width + x] = ch
                } else {
                    grid.data[y*width + x] = EMPTY
                }
            }
        }
        return grid
    }

    static Coord findStart(Grid grid){
        for(int y=0;y<grid.height;y++){
            for(int x=0;x<grid.width;x++){
                if(grid.data[y*grid.width + x] == START){
                    return new Coord(x,y)
                }
            }
        }
        return null
    }

    static class Coord{ int x; int y; Coord(int x,int y){ this.x=x; this.y=y } }

    static class Grid{ int width; int height; String[] data; Grid(int w,int h){ width=w; height=h; data=new String[w*h]; for(int i=0;i<w*h;i++) data[i]=EMPTY } }

    static class Pipe{ boolean top; boolean right; boolean bottom; boolean left; Pipe(boolean t, boolean r, boolean b, boolean l){ top=t; right=r; bottom=b; left=l } }

    static void main(String[] args){
        List<String> lines = []
        File f = new File("input.txt")
        if(!f.exists()){
            println 0
            return
        }
        f.eachLine { line -> lines << line }
        int res = solve(lines)
        println res
    }
}
class Coord { int x; int y; Coord(int x,int y){ this.x=x; this.y=y } } 
PipeSolver.main(new String[0])