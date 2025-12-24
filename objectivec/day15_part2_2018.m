#import <Foundation/Foundation.h>

#define MAX 50
#define QMAX (MAX*MAX)

typedef struct { int x,y; } Pt;
typedef struct { int k,x,y; } Tile;
typedef struct { int id,k,hp,pw,x,y,alive; } Unit;

Pt off[4]={{0,-1},{-1,0},{1,0},{0,1}};
Tile m[MAX][MAX],im[MAX][MAX];
Unit u[MAX],iu[MAX];
int R,C,N,iN;
int qh,qt,qdist[MAX][MAX];
Pt q[QMAX],qprev[MAX][MAX];

static inline void enq(Pt p){ q[qt++]=p; }
static inline Pt deq(void){ return q[qh++]; }
static inline int emp(void){ return qh==qt; }
static void rst(void){ qh=qt=0; }

static int ucmp(const void *a,const void *b){
  Unit *A=(Unit*)a,*B=(Unit*)b;
  if(A->y!=B->y) return A->y-B->y;
  return A->x-B->x;
}
static int pcmp(const void *a,const void *b){
  Pt *A=(Pt*)a,*B=(Pt*)b;
  if(A->y!=B->y) return A->y-B->y;
  return A->x-B->x;
}

static void bfs(Pt s){
  for(int y=0;y<R;y++) for(int x=0;x<C;x++){
    qdist[y][x]=-1; qprev[y][x]=(Pt){-1,-1};
  }
  rst();
  qdist[s.y][s.x]=0; enq(s);
  while(!emp()){
    Pt c=deq();
    for(int i=0;i<4;i++){
      Pt n={c.x+off[i].x,c.y+off[i].y};
      if(n.x>=0&&n.x<C&&n.y>=0&&n.y<R&&m[n.y][n.x].k==1&&qdist[n.y][n.x]==-1){
        qdist[n.y][n.x]=qdist[c.y][c.x]+1;
        qprev[n.y][n.x]=c;
        enq(n);
      }
    }
  }
}

static void load(int ep){
  FILE *f=fopen("input.txt","r");
  char ln[MAX+3];
  R=N=0;
  while(fgets(ln,sizeof(ln),f)){
    C=(int)strlen(ln)-1;
    for(int x=0;x<C;x++){
      int k;
      switch(ln[x]){
        case '#':k=8;break;
        case '.':k=1;break;
        case 'E':k=2;break;
        case 'G':k=4;break;
        default:k=1;
      }
      m[R][x]=(Tile){k,x,R};
      if(k==2||k==4){
        u[N++]=(Unit){N,k,200,ep*(k==2)+3*(k==4),x,R,1};
      }
    }
    R++;
  }
  memcpy(im,m,sizeof(m));
  memcpy(iu,u,sizeof(u));
  iN=N;
  fclose(f);
}

static void reset(void){
  memcpy(m,im,sizeof(im));
  memcpy(u,iu,sizeof(iu));
  N=iN;
  for(int i=0;i<N;i++){ u[i].alive=1; u[i].hp=200; }
}

static int status(void){
  int s=0;
  for(int i=0;i<N;i++) if(u[i].alive) s+=u[i].hp;
  return s;
}

static int tick(int stop){
  qsort(u,N,sizeof(Unit),ucmp);
  for(int i=0;i<N;i++){
    if(!u[i].alive) continue;
    Unit *cu=&u[i];
    int tk=cu->k==2?4:2,found=0;
    for(int j=0;j<N;j++) if(u[j].alive&&u[j].k==tk){found=1;break;}
    if(!found) return 0;
    Unit *at=NULL;int mh=INT_MAX;
    for(int d=0;d<4;d++){
      Pt n={cu->x+off[d].x,cu->y+off[d].y};
      if(n.x<0||n.x>=C||n.y<0||n.y>=R) continue;
      for(int j=0;j<N;j++)
        if(u[j].alive&&u[j].k==tk&&u[j].x==n.x&&u[j].y==n.y&&u[j].hp<mh){
          mh=u[j].hp; at=&u[j];
        }
    }
    if(!at){
      bfs((Pt){cu->x,cu->y});
      Pt tgt[MAX*4];int tc=0,md=INT_MAX;
      for(int j=0;j<N;j++){
        if(!u[j].alive||u[j].k!=tk) continue;
        for(int d=0;d<4;d++){
          Pt n={u[j].x+off[d].x,u[j].y+off[d].y};
          if(n.x<0||n.x>=C||n.y<0||n.y>=R||m[n.y][n.x].k!=1) continue;
          int d0=qdist[n.y][n.x];
          if(d0!=-1&&d0<md){md=d0;tc=0;tgt[tc++]=n;}
          else if(d0==md) tgt[tc++]=n;
        }
      }
      if(tc>0){
        qsort(tgt,tc,sizeof(Pt),pcmp);
        Pt st=tgt[0],step=st;
        while(qdist[step.y][step.x]>1) step=qprev[step.y][step.x];
        if(qdist[step.y][step.x]==1){
          m[cu->y][cu->x].k=1;
          cu->x=step.x;cu->y=step.y;
          m[cu->y][cu->x].k=cu->k;
        }
        at=NULL;mh=INT_MAX;
        for(int d=0;d<4;d++){
          Pt n={cu->x+off[d].x,cu->y+off[d].y};
          if(n.x<0||n.x>=C||n.y<0||n.y>=R) continue;
          for(int j=0;j<N;j++)
            if(u[j].alive&&u[j].k==tk&&u[j].x==n.x&&u[j].y==n.y&&u[j].hp<mh){
              mh=u[j].hp;at=&u[j];
            }
        }
      }
    }
    if(at){
      at->hp-=cu->pw;
      if(at->hp<=0){
        at->alive=0;
        m[at->y][at->x].k=1;
        if(stop&&at->k==2) return -1;
      }
    }
  }
  int w=0;
  for(int i=0;i<N;i++) if(u[i].alive) u[w++]=u[i];
  N=w;
  return 1;
}

int main(void){
  int ep=3,out=0;
  while(1){
    ep++;
    load(ep);
    int rnd=0,ok=1;
    while(1){
      int r=tick(1);
      if(r==-1){ok=0;break;}
      if(r==0){out=rnd*status();break;}
      rnd++;
    }
    if(ok){printf("%d\n",out);break;}
  }
  return 0;
}