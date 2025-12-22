
#import <Foundation/Foundation.h>

#define MAXN 150
#define MAXM 150
#define HEAP_CAP (MAXN*MAXM*4+1)

typedef struct { int cost,x,y,d; } State;
typedef struct { int x,y,d; } PointDir;

static char grid[MAXN][MAXM];
static int dist[MAXN][MAXM][4];
static bool vis[MAXN][MAXM][4], used[MAXN][MAXM];
static State heap[HEAP_CAP];
static int hsz, rev_top;
static PointDir revStack[HEAP_CAP];
static int N,M,sx,sy,ex,ey;
static const int dx[] = {-1,0,1,0}, dy[] = {0,1,0,-1};

static inline void swap(State *a,State *b){ State t=*a;*a=*b;*b=t; }
static void hpush(State s){
    heap[hsz]=s;
    int i=hsz++;
    while(i){
        int p=(i-1)/2;
        if(heap[i].cost>=heap[p].cost) break;
        swap(&heap[i],&heap[p]); i=p;
    }
}
static State hpop(void){
    State top=heap[0];
    heap[0]=heap[--hsz];
    int i=0;
    while(1){
        int l=2*i+1,r=l+1,best=i;
        if(l<hsz && heap[l].cost<heap[best].cost) best=l;
        if(r<hsz && heap[r].cost<heap[best].cost) best=r;
        if(best==i) break;
        swap(&heap[i],&heap[best]); i=best;
    }
    return top;
}

int main(int argc,char **argv){
    @autoreleasepool {
        NSString *path = [[NSBundle mainBundle] pathForResource:@"input" ofType:@"txt"];
        if(!path) path = @"input.txt";
        NSString *whole = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if(!whole){ printf("0\n"); return 0; }
        NSArray *lines = [whole componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        N=0;
        for(NSString *l in lines){
            if(l.length==0) continue;
            M=(int)l.length;
            for(int j=0;j<M;j++){
                char c=[l characterAtIndex:j];
                grid[N][j]=c;
                if(c=='S'){ sx=N; sy=j; }
                else if(c=='E'){ ex=N; ey=j; }
            }
            N++;
            if(N>=MAXN) break;
        }
        if(N==0||M==0||sx<0||ex<0){ printf("0\n"); return 0; }

        for(int i=0;i<N;i++) for(int j=0;j<M;j++){
            used[i][j]=false;
            for(int d=0;d<4;d++){ dist[i][j][d]=INT_MAX; vis[i][j][d]=false; }
        }

        dist[sx][sy][1]=0;
        hpush((State){0,sx,sy,1});
        while(hsz){
            State c=hpop();
            int x=c.x,y=c.y,d=c.d,cost=c.cost;
            if(cost>dist[x][y][d]) continue;
            for(int turn=0;turn<2;turn++){
                int nd=(d+(turn?3:1))&3,nc=cost+1000;
                if(nc<dist[x][y][nd]){ dist[x][y][nd]=nc; hpush((State){nc,x,y,nd}); }
            }
            int nx=x+dx[d],ny=y+dy[d];
            if(nx>=0&&nx<N&&ny>=0&&ny<M&&grid[nx][ny]!='#'){
                int nc=cost+1;
                if(nc<dist[nx][ny][d]){ dist[nx][ny][d]=nc; hpush((State){nc,nx,ny,d}); }
            }
        }

        int best=INT_MAX;
        for(int d=0;d<4;d++) if(dist[ex][ey][d]<best) best=dist[ex][ey][d];
        if(best==INT_MAX){ printf("0\n"); return 0; }

        rev_top=0;
        for(int d=0;d<4;d++) if(dist[ex][ey][d]==best) revStack[rev_top++]=(PointDir){ex,ey,d}, vis[ex][ey][d]=true;
        while(rev_top){
            PointDir c=revStack[--rev_top];
            int x=c.x,y=c.y,d=c.d,cost=dist[x][y][d];
            used[x][y]=true;
            for(int turn=0;turn<2;turn++){
                int pd=(d+(turn?1:3))&3;
                if(cost>=1000 && dist[x][y][pd]==cost-1000 && !vis[x][y][pd]){
                    vis[x][y][pd]=true; revStack[rev_top++]=(PointDir){x,y,pd};
                }
            }
            int px=x-dx[d],py=y-dy[d];
            if(px>=0&&px<N&&py>=0&&py<M&&grid[px][py]!='#'){
                if(cost>0 && dist[px][py][d]==cost-1 && !vis[px][py][d]){
                    vis[px][py][d]=true; revStack[rev_top++]=(PointDir){px,py,d};
                }
            }
        }

        int cnt=0;
        for(int i=0;i<N;i++) for(int j=0;j<M;j++) if(used[i][j]) cnt++;
        printf("%d\n",cnt);
    }
    return 0;
}
