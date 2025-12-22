
#import <Foundation/Foundation.h>

typedef struct {int x,y,vx,vy;} Robot;

static inline int mod(int a,int b){int r=a%b;return r<0?r+b:r;}
static inline Robot parseLine(NSString *line){
    Robot r; sscanf([line UTF8String],"p=%d,%d v=%d,%d",&r.x,&r.y,&r.vx,&r.vy); return r;
}
static void moveRobots(Robot *a,int n,int sx,int sy){
    for(int i=0;i<n;i++){a[i].x=mod(a[i].x+a[i].vx,sx);a[i].y=mod(a[i].y+a[i].vy,sy);}
}
static int countQuadrants(Robot *a,int n,int sx,int sy){
    int c[4]={0};int cx=sx/2,cy=sy/2;
    for(int i=0;i<n;i++){
        int x=a[i].x,y=a[i].y;
        if(x<cx){
            if(y<cy)c[0]++;else if(y>cy)c[1]++;
        }else if(x>cx){
            if(y<cy)c[2]++;else if(y>cy)c[3]++;
        }
    }
    int f=1;for(int i=0;i<4;i++)f*=c[i];return f;
}
static int hasNoOverlaps(Robot *a,int n){
    static char occ[101][103];
    memset(occ,0,sizeof(occ));
    for(int i=0;i<n;i++)if(occ[a[i].x][a[i].y]++)return 0;
    return 1;
}
static void drawGrid(Robot *a,int n,int sx,int sy){
    static char g[103][101];
    for(int y=0;y<sy;y++)for(int x=0;x<sx;x++)g[y][x]='.';
    for(int i=0;i<n;i++)g[a[i].y][a[i].x]='#';
    for(int y=0;y<sy;y++){
        fwrite(g[y],1,sx,stdout);
        putchar('\n');
    }
}

int main(int argc,const char * argv[]){
    @autoreleasepool{
        int sx=101,sy=103;
        NSMutableArray *lines=[NSMutableArray array];
        NSString *path=[[NSBundle mainBundle] pathForResource:@"input" ofType:@"txt"];
        if(!path)path=@"input.txt";
        NSString *content=[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        for(NSString *l in [content componentsSeparatedByString:@"\n"])
            if(l.length) [lines addObject:l];
        int n=lines.count;
        Robot robots[1000];
        for(int i=0;i<n;i++)robots[i]=parseLine(lines[i]);
        Robot part1[1000]; memcpy(part1,robots,sizeof(Robot)*n);
        for(int t=0;t<100;t++)moveRobots(part1,n,sx,sy);
        printf("Part 1 - Safety Factor after 100 seconds: %d\n",countQuadrants(part1,n,sx,sy));
        Robot part2[1000]; memcpy(part2,robots,sizeof(Robot)*n);
        int sec=0;
        while(!hasNoOverlaps(part2,n)){
            moveRobots(part2,n,sx,sy);
            sec++;
            if(sec>1000000){printf("Exceeded maximum iterations.\n"); return 1;}
        }
        printf("Part 2 - Fewest seconds to display Easter egg: %d\n",sec);
        printf("Final positions of robots:\n");
        drawGrid(part2,n,sx,sy);
    }
    return 0;
}
