
#import <Foundation/Foundation.h>
#import <limits.h>
static int min(int a,int b){return a<b?a:b;}
static void bfs(char **grid,int rows,int cols,int sr,int sc,int *dist,int pc){
    int max=rows*cols;
    int (*q)[3]=malloc(max*sizeof(*q));
    int *vis=calloc(rows*cols,sizeof(int));
    int head=0,tail=0;
    q[tail][0]=sr;q[tail][1]=sc;q[tail][2]=0;tail++;
    vis[sr*cols+sc]=1;
    int dr[4]={0,0,1,-1},dc[4]={-1,1,0,0};
    while(head<tail){
        int r=q[head][0],c=q[head][1],d=q[head][2];head++;
        char ch=grid[r][c];
        if(ch>='0'&&ch<='9')dist[ch-'0']=d;
        for(int i=0;i<4;i++){
            int nr=r+dr[i],nc=c+dc[i];
            if(nr>=0&&nr<rows&&nc>=0&&nc<cols&&grid[nr][nc]!='#'&&!vis[nr*cols+nc]){
                vis[nr*cols+nc]=1;
                q[tail][0]=nr;q[tail][1]=nc;q[tail][2]=d+1;tail++;
            }
        }
    }
    free(q);free(vis);
}
static int dfs(int **g,int e,int *vis,int pc,int retZero){
    int vc=0;
    for(int i=0;i<pc;i++)if(vis[i])vc++;
    if(vc==pc)return retZero?g[e][0]:0;
    int best=INT_MAX;
    for(int i=0;i<pc;i++)if(!vis[i]){
        vis[i]=1;
        int cur=g[e][i]+dfs(g,i,vis,pc,retZero);
        best=min(best,cur);
        vis[i]=0;
    }
    return best;
}
int main(int argc,const char *argv[]){
    @autoreleasepool{
        NSString *path=@"input.txt";
        NSArray *lines=[[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil] componentsSeparatedByString:@"\n"];
        int rows=0,cols=0;
        char **grid=malloc(1000*sizeof(*grid));
        for(int i=0;i<1000;i++)grid[i]=malloc(1000);
        for(NSString *ln in lines){
            if(!ln.length)continue;
            const char *cstr=[ln UTF8String];
            strcpy(grid[rows],cstr);
            cols=strlen(grid[rows]);
            rows++;
        }
        int pc=0;
        for(int r=0;r<rows;r++)for(int c=0;c<cols;c++)if(grid[r][c]>='0'&&grid[r][c]<='9')pc++;
        int **g=malloc(pc*sizeof(*g));
        for(int i=0;i<pc;i++)g[i]=malloc(pc*sizeof(int));
        for(int r=0;r<rows;r++)for(int c=0;c<cols;c++)if(grid[r][c]>='0'&&grid[r][c]<='9'){
            int d[10]={0};
            bfs(grid,rows,cols,r,c,d,pc);
            int idx=grid[r][c]-'0';
            for(int i=0;i<pc;i++)g[idx][i]=d[i];
        }
        int *vis=calloc(pc,sizeof(int));
        vis[0]=1;
        int ans=dfs(g,0,vis,pc,1);
        printf("%d\n",ans);
        for(int i=0;i<1000;i++)free(grid[i]);free(grid);
        for(int i=0;i<pc;i++)free(g[i]);free(g);
        free(vis);
    }
    return 0;
}
