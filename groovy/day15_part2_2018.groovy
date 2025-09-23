import java.util.*
class P{
    static final int KIND_SPACE=1,KIND_ELF=2,KIND_GOBLIN=4,KIND_WALL=8
    static class Unit{int id,kind,hp,power,x,y;boolean alive}
    static class Pt{int x,y;Pt(int x,int y){this.x=x;this.y=y}}
    static int[] offx=[0,-1,1,0]
    static int[] offy=[-1,0,0,1]
    static void main(String[] a){
        def lines = new File("input.txt").readLines()
        int rows = lines.size()
        int cols = lines[0].length()
        char[][] initMap = new char[rows][]
        List<Unit> initUnits = []
        int uid=0
        for(int y=0;y<rows;y++){
            initMap[y]=lines[y].toCharArray()
            for(int x=0;x<initMap[y].length;x++){
                char c = initMap[y][x]
                if(c=='E' || c=='G'){
                    Unit u=new Unit(id:uid++,kind:(c=='E'?KIND_ELF:KIND_GOBLIN),hp:200,power:(c=='E'?3:3),x:x,y:y,alive:true)
                    initUnits.add(u)
                }
            }
        }
        int elfPower=3
        int finalOutcome=0
        while(true){
            elfPower++
            char[][] map = new char[rows][]
            for(int y=0;y<rows;y++){
                map[y]=initMap[y].clone()
            }
            List<Unit> units = initUnits.collect{ new Unit(id:it.id,kind:it.kind,hp:200,power:(it.kind==KIND_ELF?elfPower:3),x:it.x,y:it.y,alive:true) }
            int rounds=0
            boolean elfDied=false
            while(true){
                int hpSum = getStatus(units)
                if(hpSum==0){ finalOutcome = rounds * 0; break }
                boolean[] elfDiedArr = new boolean[1]
                boolean roundCompleted = tick(units,map,rows,cols,true,elfDiedArr)
                if(elfDiedArr[0]){ elfDied=true; break }
                if(!roundCompleted && !elfDied){
                    hpSum = getStatus(units)
                    finalOutcome = rounds * hpSum
                    break
                }
                rounds++
            }
            if(!elfDied) break
        }
        println finalOutcome
    }
    static int getStatus(List<Unit> units){
        int sum=0
        boolean e=false,g=false
        for(u in units) if(u.alive){ sum+=u.hp; if(u.kind==KIND_ELF) e=true; else g=true }
        if(!e || !g) return sum
        return sum
    }
    static boolean tick(List<Unit> units, char[][] map, int rows, int cols, boolean stopOnElfDeath, boolean[] elfDiedOut){
        units.sort{ a,b -> a.y<=>b.y ?: a.x<=>b.x }
        elfDiedOut[0]=false
        for(int i=0;i<units.size();i++){
            Unit cur = units[i]
            if(!cur.alive) continue
            int targetKind = (cur.kind==KIND_ELF)?KIND_GOBLIN:KIND_ELF
            boolean targetsExist=false
            for(u in units) if(u.alive && u.kind==targetKind){ targetsExist=true; break }
            if(!targetsExist) return false
            Unit attackTarget = null
            int minHp = Integer.MAX_VALUE
            for(int k=0;k<4;k++){
                int nx=cur.x+offx[k], ny=cur.y+offy[k]
                if(nx>=0 && nx<cols && ny>=0 && ny<rows){
                    for(u in units){
                        if(u.alive && u.kind==targetKind && u.x==nx && u.y==ny){
                            if(u.hp<minHp){ minHp=u.hp; attackTarget=u }
                            break
                        }
                    }
                }
            }
            if(attackTarget==null){
                def bfsRes = bfs(cur,map,rows,cols)
                int[][] dist = bfsRes[0]
                Pt[][] prev = bfsRes[1]
                List<Pt> targets = []
                int minDist = Integer.MAX_VALUE
                for(u in units) if(u.alive && u.kind==targetKind){
                    for(int k=0;k<4;k++){
                        int nx=u.x+offx[k], ny=u.y+offy[k]
                        if(nx>=0 && nx<cols && ny>=0 && ny<rows && map[ny][nx]=='.'){
                            int d = dist[ny][nx]
                            if(d!=-1){
                                if(d<minDist){ minDist=d; targets.clear(); targets.add(new Pt(nx,ny)) }
                                else if(d==minDist) targets.add(new Pt(nx,ny))
                            }
                        }
                    }
                }
                if(targets.size()>0){
                    targets.sort{ a,b -> a.y<=>b.y ?: a.x<=>b.x }
                    Pt chosen = targets[0]
                    Pt step = chosen
                    while(dist[step.y][step.x]>1){
                        step = prev[step.y][step.x]
                        if(step==null) break
                    }
                    if(step!=null && dist[step.y][step.x]==1){
                        map[cur.y][cur.x]='.'
                        cur.x=step.x; cur.y=step.y
                        map[cur.y][cur.x] = (cur.kind==KIND_ELF?'E':'G')
                    }
                    minHp = Integer.MAX_VALUE
                    attackTarget = null
                    for(int k=0;k<4;k++){
                        int nx=cur.x+offx[k], ny=cur.y+offy[k]
                        if(nx>=0 && nx<cols && ny>=0 && ny<rows){
                            for(u in units){
                                if(u.alive && u.kind==targetKind && u.x==nx && u.y==ny){
                                    if(u.hp<minHp){ minHp=u.hp; attackTarget=u }
                                    break
                                }
                            }
                        }
                    }
                }
            }
            if(attackTarget!=null){
                attackTarget.hp -= cur.power
                if(attackTarget.hp<=0){
                    attackTarget.alive=false
                    map[attackTarget.y][attackTarget.x]='.'
                    if(attackTarget.kind==KIND_ELF && stopOnElfDeath){
                        elfDiedOut[0]=true
                        return true
                    }
                }
            }
        }
        List<Unit> live = []
        for(u in units) if(u.alive) live.add(u)
        units.clear(); units.addAll(live)
        return true
    }
    static def bfs(Unit cur, char[][] map, int rows, int cols){
        int[][] dist = new int[rows][cols]
        Pt[][] prev = new Pt[rows][cols]
        for(int y=0;y<rows;y++) for(int x=0;x<cols;x++){ dist[y][x]=-1; prev[y][x]=null }
        ArrayDeque<Pt> q = new ArrayDeque<>()
        dist[cur.y][cur.x]=0
        q.add(new Pt(cur.x,cur.y))
        while(!q.isEmpty()){
            Pt p = q.removeFirst()
            for(int i=0;i<4;i++){
                int nx=p.x+offx[i], ny=p.y+offy[i]
                if(nx>=0 && nx<cols && ny>=0 && ny<rows && map[ny][nx]=='.' && dist[ny][nx]==-1){
                    dist[ny][nx]=dist[p.y][p.x]+1
                    prev[ny][nx]=p
                    q.add(new Pt(nx,ny))
                }
            }
        }
        return [dist,prev]
    }
}