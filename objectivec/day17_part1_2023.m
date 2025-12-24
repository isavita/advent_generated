
#import <Foundation/Foundation.h>

#define MAX_STRAIGHT 3
#define MIN_STRAIGHT 0

typedef struct { int x,y; } Coord;
static inline Coord C(int x,int y){ return (Coord){x,y}; }
static inline int h(Coord a,Coord b){ return abs(a.x-b.x)+abs(a.y-b.y); }

typedef struct { int pri,cost; Coord pos,dir; int run; } Item;

typedef struct { Item *q; int n,cap; } PQ;
static PQ* PQnew(int cap){
    PQ*p=malloc(sizeof(PQ)); p->q=malloc(cap*sizeof(Item)); p->n=0; p->cap=cap; return p;
}
static void PQpush(PQ*p,Item it){
    if(p->n==p->cap){ p->cap*=2; p->q=realloc(p->q,p->cap*sizeof(Item)); }
    int i=p->n++; p->q[i]=it;
    while(i>0){ int par=(i-1)/2; if(p->q[par].pri<=p->q[i].pri)break; Item t=p->q[i];p->q[i]=p->q[par];p->q[par]=t; i=par; }
}
static Item PQpop(PQ*p){
    Item top=p->q[0]; p->q[0]=p->q[--p->n];
    int i=0;
    while(1){
        int l=2*i+1,r=l+1,m=i;
        if(l<p->n && p->q[l].pri<p->q[m].pri) m=l;
        if(r<p->n && p->q[r].pri<p->q[m].pri) m=r;
        if(m==i)break;
        Item t=p->q[i];p->q[i]=p->q[m];p->q[m]=t; i=m;
    }
    return top;
}

int main(){
    NSString *txt=[NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
    NSArray *rows=[txt componentsSeparatedByString:@"\n"];
    int H=(int)rows.count,W=(int)[rows[0] length];
    char grid[H][W];
    for(int y=0;y<H;y++){
        NSString *line=rows[y];
        for(int x=0;x<W;x++) grid[y][x]=[line characterAtIndex:x]-'0';
    }
    int best[H][W][5][MAX_STRAIGHT+1];
    for(int y=0;y<H;y++)for(int x=0;x<W;x++)for(int d=0;d<5;d++)for(int r=0;r<=MAX_STRAIGHT;r++) best[y][x][d][r]=INT_MAX;
    PQ *pq=PQnew(1024);
    PQpush(pq,(Item){h(C(0,0),C(W-1,H-1)),0,C(0,0),C(0,0),0});
    int ans=INT_MAX;
    while(pq->n){
        Item it=PQpop(pq);
        if(it.cost>=ans) continue;
        int dix=4;
        if(it.dir.x) dix= it.dir.x==1?3:1;
        else if(it.dir.y) dix= it.dir.y==1?2:0;
        if(it.cost>=best[it.pos.y][it.pos.x][dix][it.run]) continue;
        best[it.pos.y][it.pos.x][dix][it.run]=it.cost;
        if(it.pos.x==W-1 && it.pos.y==H-1){ ans=it.cost; continue; }
        Coord D[]={C(0,-1),C(-1,0),C(0,1),C(1,0)};
        for(int k=0;k<4;k++){
            Coord d=D[k],np=C(it.pos.x+d.x,it.pos.y+d.y);
            if(np.x<0||np.x>=W||np.y<0||np.y>=H) continue;
            if(it.run && d.x==-it.dir.x && d.y==-it.dir.y) continue;
            int nr= (d.x==it.dir.x && d.y==it.dir.y)?it.run+1:1;
            if(nr>MAX_STRAIGHT) continue;
            if(it.run<MIN_STRAIGHT && !(d.x==it.dir.x && d.y==it.dir.y)) continue;
            int nc=it.cost+grid[np.y][np.x];
            int ndix=4;
            if(d.x) ndix= d.x==1?3:1;
            else if(d.y) ndix= d.y==1?2:0;
            if(nc>=best[np.y][np.x][ndix][nr]) continue;
            PQpush(pq,(Item){nc+h(np,C(W-1,H-1)),nc,np,d,nr});
        }
    }
    printf("%d\n",ans);
    return 0;
}
