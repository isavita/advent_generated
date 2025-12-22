#import <Foundation/Foundation.h>

#define MAX 1000
#define QMAX (MAX*MAX*4)

typedef struct { int cost, r, c, dir } State;
static char g[MAX][MAX+1];
static bool v[MAX][MAX][4];
static State q[QMAX];
static int qs, top;

static void push(State s) {
    int i = qs++;
    for (; i && q[i-1>>1].cost > s.cost; i = i-1>>1)
        q[i] = q[i-1>>1];
    q[i] = s;
}
static State pop(void) {
    State ret = q[0], s = q[--qs];
    int i = 0, j;
    while ((j = i*2+1) < qs) {
        if (j+1<qs && q[j+1].cost<q[j].cost) j++;
        if (s.cost <= q[j].cost) break;
        q[i] = q[j]; i = j;
    }
    q[i] = s;
    return ret;
}

int main(int argc, char **argv) {
    @autoreleasepool {
        NSString *f = [NSString stringWithContentsOfFile:@"input.txt"
                                                 encoding:NSUTF8StringEncoding error:nil];
        if (!f) return 1;
        NSArray *lines = [f componentsSeparatedByString:@"\n"];
        int R=0, C=0, sr=0, sc=0, er=0, ec=0;
        for (NSString *l in lines) {
            if ([l length]==0) continue;
            C = (int)[l length];
            for (int i=0;i<C;i++){
                g[R][i] = [l characterAtIndex:i];
                if (g[R][i]=='S') { sr=R; sc=i; }
                if (g[R][i]=='E') { er=R; ec=i; }
            }
            g[R][C]=0; R++;
        }
        int dr[]={0,1,0,-1}, dc[]={1,0,-1,0};
        push((State){0,sr,sc,0});
        while (qs) {
            State c = pop();
            if (v[c.r][c.c][c.dir]) continue;
            v[c.r][c.c][c.dir]=1;
            if (c.r==er && c.c==ec) { printf("%d\n",c.cost); return 0; }
            int nr=c.r+dr[c.dir], nc=c.c+dc[c.dir];
            if (nr>=0 && nr<R && nc>=0 && nc<C && g[nr][nc]!='#')
                push((State){c.cost+1,nr,nc,c.dir});
            push((State){c.cost+1000,c.r,c.c,(c.dir+1)%4});
            push((State){c.cost+1000,c.r,c.c,(c.dir+3)%4});
        }
        return 1;
    }
}