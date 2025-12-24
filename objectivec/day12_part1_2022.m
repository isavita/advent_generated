
#import <Foundation/Foundation.h>

#define MAX 200
int grid[MAX][MAX], dist[MAX][MAX], R, C;
typedef struct { int x, y, pri; } Item;
Item pq[MAX*MAX]; int qsz;

void sw(int i, int j){ Item t=pq[i]; pq[i]=pq[j]; pq[j]=t; }
void up(int i){
    while(i&&pq[(i-1)/2].pri<pq[i].pri){ sw(i,(i-1)/2); i=(i-1)/2; }
}
void down(int i){
    int b=i, l, r;
    while(1){
        l=2*i+1; r=2*i+2;
        if(l<qsz&&pq[l].pri>pq[b].pri) b=l;
        if(r<qsz&&pq[r].pri>pq[b].pri) b=r;
        if(b==i) break;
        sw(i,b); i=b;
    }
}
void push(int x, int y, int pri){
    pq[qsz]=(Item){x,y,pri};
    up(qsz++);
}
Item pop(void){
    Item r=pq[0];
    pq[0]=pq[--qsz];
    down(0);
    return r;
}
int ok(int x,int y){return x>=0&&x<C&&y>=0&&y<R;}
int dx[]={0,0,1,-1}, dy[]={1,-1,0,0};

int dijkstra(int ex,int ey,int sx,int sy){
    for(int i=0;i<R;i++)for(int j=0;j<C;j++)dist[i][j]=INT_MAX;
    qsz=0;
    dist[ey][ex]=0;
    push(ex,ey,0);
    while(qsz){
        Item c=pop();
        if(c.pri>dist[c.y][c.x])continue;
        for(int k=0;k<4;k++){
            int nx=c.x+dx[k], ny=c.y+dy[k];
            if(ok(nx,ny) && grid[c.y][c.x]-grid[ny][nx]<=1){
                int nd=c.pri+1;
                if(nd<dist[ny][nx]){
                    dist[ny][nx]=nd;
                    push(nx,ny,nd);
                }
            }
        }
    }
    return dist[sy][sx];
}

int main(int argc,char**argv){
    @autoreleasepool{
        NSError*e=nil;
        NSString*d=[NSString stringWithContentsOfFile:@"input.txt"
                                               encoding:NSUTF8StringEncoding
                                                  error:&e];
        if(!d){NSLog(@"%@",e);return 1;}
        int sx=0,sy=0,ex=0,ey=0;
        R=0;
        for(NSString*l in[d componentsSeparatedByString:@"\n"]){
            if(l.length==0)continue;
            C=(int)l.length;
            for(int x=0;x<C;x++){
                unichar ch=[l characterAtIndex:x];
                grid[R][x]=ch;
                if(ch=='S'){sx=x;sy=R;grid[R][x]='a';}
                if(ch=='E'){ex=x;ey=R;grid[R][x]='z';}
            }
            R++;
        }
        printf("%d\n",dijkstra(ex,ey,sx,sy));
    }
    return 0;
}
