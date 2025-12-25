
#import <Foundation/Foundation.h>

typedef struct {int r,c;} Cell;

static int dr[4] = {1,-1,0,0};
static int dc[4] = {0,0,1,-1};

int **allocDist(int H,int W){
    int **d = malloc(sizeof(int*)*H);
    for(int i=0;i<H;i++){
        d[i]=malloc(sizeof(int)*W);
        for(int j=0;j<W;j++) d[i][j]=-1;
    }
    return d;
}
void freeDist(int **d,int H){
    for(int i=0;i<H;i++) free(d[i]);
    free(d);
}
int **bfs(int H,int W,char **walls,Cell start){
    int **dist=allocDist(H,W);
    Cell *q=malloc(sizeof(Cell)*H*W);
    int head=0,tail=0;
    dist[start.r][start.c]=0;
    q[tail++]=start;
    while(head<tail){
        Cell cur=q[head++];
        int cd=dist[cur.r][cur.c];
        for(int k=0;k<4;k++){
            int nr=cur.r+dr[k], nc=cur.c+dc[k];
            if(nr<0||nr>=H||nc<0||nc>=W) continue;
            if(walls[nr][nc]) continue;
            if(dist[nr][nc]!=-1) continue;
            dist[nr][nc]=cd+1;
            q[tail++]=(Cell){nr,nc};
        }
    }
    free(q);
    return dist;
}
int **cheatBFS(int H,int W,Cell start){
    int **dist=allocDist(H,W);
    Cell *q=malloc(sizeof(Cell)*H*W);
    int head=0,tail=0;
    dist[start.r][start.c]=0;
    q[tail++]=start;
    while(head<tail){
        Cell cur=q[head++];
        int cd=dist[cur.r][cur.c];
        if(cd==20) continue;
        for(int k=0;k<4;k++){
            int nr=cur.r+dr[k], nc=cur.c+dc[k];
            if(nr<0||nr>=H||nc<0||nc>=W) continue;
            if(dist[nr][nc]!=-1) continue;
            dist[nr][nc]=cd+1;
            q[tail++]=(Cell){nr,nc};
        }
    }
    free(q);
    return dist;
}

int main(int argc, const char * argv[]){
    @autoreleasepool{
        NSString *content=[NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *raw=[content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray *lines=[NSMutableArray array];
        for(NSString *s in raw) if(s.length) [lines addObject:s];
        int H=(int)lines.count;
        if(H==0){ printf("0\n"); return 0; }
        int W=(int)[lines[0] length];
        char **grid=malloc(sizeof(char*)*H);
        char **walls=malloc(sizeof(char*)*H);
        NSMutableArray *trackCells=[NSMutableArray array];
        Cell S,E;
        for(int i=0;i<H;i++){
            NSString *row=lines[i];
            grid[i]=malloc(W+1);
            walls[i]=malloc(W);
            memcpy(grid[i],[row UTF8String],W);
            grid[i][W]='\0';
            for(int j=0;j<W;j++){
                char ch=grid[i][j];
                walls[i][j]=(ch=='#');
                if(ch!='#'){
                    [trackCells addObject:[NSValue valueWithBytes:&(Cell){i,j} objCType:@encode(Cell)]];
                }
                if(ch=='S') S=(Cell){i,j};
                if(ch=='E') E=(Cell){i,j};
            }
        }
        int **distS=bfs(H,W,walls,S);
        int **distE=bfs(H,W,walls,E);
        int normal=distS[E.r][E.c];
        if(normal==-1){ printf("0\n"); return 0; }
        NSMutableDictionary *cheats=[NSMutableDictionary dictionary];
        for(NSValue *val in trackCells){
            Cell start;
            [val getValue:&start];
            int sd=distS[start.r][start.c];
            if(sd==-1) continue;
            int **distC=cheatBFS(H,W,start);
            for(int r=0;r<H;r++) for(int c=0;c<W;c++){
                int s=distC[r][c];
                if(s<=0||s>20||walls[r][c]) continue;
                int ed=distE[r][c];
                if(ed==-1) continue;
                int cost=sd+s+ed;
                if(cost<normal){
                    NSString *key=[NSString stringWithFormat:@"%d,%d,%d,%d",start.r,start.c,r,c];
                    NSNumber *prev=cheats[key];
                    if(!prev||cost<[prev intValue]) cheats[key]=@(cost);
                }
            }
            freeDist(distC,H);
        }
        int count=0;
        for(NSNumber *val in cheats.allValues){
            if(normal-[val intValue]>=100) count++;
        }
        printf("%d\n",count);
        freeDist(distS,H);
        freeDist(distE,H);
        for(int i=0;i<H;i++){
            free(grid[i]); free(walls[i]);
        }
        free(grid); free(walls);
    }
    return 0;
}
