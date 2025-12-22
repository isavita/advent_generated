
#import <Foundation/Foundation.h>

#define SIZE 71
#define QMAX (SIZE*SIZE)

@interface Solver : NSObject
- (BOOL)canReach:(BOOL[SIZE][SIZE])grid;
- (void)run;
@end

@implementation Solver

- (BOOL)canReach:(BOOL[SIZE][SIZE])grid {
    if (grid[0][0] || grid[SIZE-1][SIZE-1]) return NO;
    BOOL vis[SIZE][SIZE];
    memset(vis, 0, sizeof(vis));
    int qx[QMAX], qy[QMAX], head=0, tail=0;
    qx[tail]=0; qy[tail]=0; tail++;
    vis[0][0]=YES;
    int dx[]={1,-1,0,0}, dy[]={0,0,1,-1};
    while(head<tail){
        int x=qx[head], y=qy[head]; head++;
        if(x==SIZE-1 && y==SIZE-1) return YES;
        for(int i=0;i<4;i++){
            int nx=x+dx[i], ny=y+dy[i];
            if(nx>=0 && ny>=0 && nx<SIZE && ny<SIZE && !grid[ny][nx] && !vis[ny][nx]){
                vis[ny][nx]=YES;
                qx[tail]=nx; qy[tail]=ny; tail++;
            }
        }
    }
    return NO;
}

- (void)run {
    NSString *txt=[NSString stringWithContentsOfFile:@"input.txt"
                                              encoding:NSUTF8StringEncoding error:nil];
    BOOL grid[SIZE][SIZE];
    memset(grid, 0, sizeof(grid));
    for(NSString *line in [txt componentsSeparatedByString:@"\n"]){
        NSArray *p=[line componentsSeparatedByString:@","];
        if(p.count!=2) continue;
        int x=[p[0] intValue], y=[p[1] intValue];
        if(x<0||x>=SIZE||y<0||y>=SIZE) continue;
        grid[y][x]=YES;
        if(![self canReach:grid]){
            printf("%d,%d\n",x,y);
            return;
        }
    }
    printf("No cutoff found\n");
}

@end

int main(int argc, char *argv[]) {
    @autoreleasepool {
        [[Solver new] run];
    }
    return 0;
}
