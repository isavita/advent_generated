
#import <Foundation/Foundation.h>
#define MAX_WIDTH 200
#define MAX_HEIGHT 200
#define MAX_STEPS 10
#define HEAP_CAPACITY (MAX_WIDTH * MAX_HEIGHT * 4 * (MAX_STEPS + 1))

typedef enum {N,S,E,W} Direction;
typedef struct {int cost,r,c;Direction dir;int steps;} State;
static State heap[HEAP_CAPACITY];
static int heapSize=0;
static inline void swap(State *a,State *b){State t=*a;*a=*b;*b=t;}
static inline void heapPush(State s){
    heap[heapSize]=s;
    int i=heapSize++;
    while(i){
        int p=(i-1)/2;
        if(heap[p].cost<=heap[i].cost)break;
        swap(&heap[p],&heap[i]);i=p;
    }
}
static inline State heapPop(){
    State top=heap[0];
    heap[0]=heap[--heapSize];
    int i=0;
    while(1){
        int l=2*i+1,r=2*i+2,sm=i;
        if(l<heapSize&&heap[l].cost<heap[sm].cost)sm=l;
        if(r<heapSize&&heap[r].cost<heap[sm].cost)sm=r;
        if(sm==i)break;
        swap(&heap[i],&heap[sm]);i=sm;
    }
    return top;
}
static char grid[MAX_HEIGHT][MAX_WIDTH+2];
static int height=0,width=0;
static int dist[MAX_HEIGHT][MAX_WIDTH][4][MAX_STEPS+1];
static int dr[4]={-1,1,0,0};
static int dc[4]={0,0,1,-1};

int solve(int minTurn,int maxStraight){
    for(int r=0;r<height;r++)for(int c=0;c<width;c++)for(int d=0;d<4;d++)for(int s=0;s<=maxStraight;s++)dist[r][c][d][s]=INT_MAX;
    heapSize=0;
    if(width>1){
        int cost=grid[0][1]-'0';
        dist[0][1][E][1]=cost;
        heapPush((State){cost,0,1,E,1});
    }
    if(height>1){
        int cost=grid[1][0]-'0';
        dist[1][0][S][1]=cost;
        heapPush((State){cost,1,0,S,1});
    }
    while(heapSize){
        State cur=heapPop();
        if(cur.cost>dist[cur.r][cur.c][cur.dir][cur.steps])continue;
        if(cur.r==height-1&&cur.c==width-1&&cur.steps>=minTurn)return cur.cost;
        for(Direction nd=N;nd<=W;nd++){
            if((cur.dir==N&&nd==S)||(cur.dir==S&&nd==N)||(cur.dir==E&&nd==W)||(cur.dir==W&&nd==E))continue;
            int nr=cur.r+dr[nd],nc=cur.c+dc[nd];
            if(nr<0||nr>=height||nc<0||nc>=width)continue;
            int ncost=cur.cost+(grid[nr][nc]-'0');
            int nsteps;
            if(nd==cur.dir){
                if(cur.steps>=maxStraight)continue;
                nsteps=cur.steps+1;
            }else{
                if(cur.steps<minTurn)continue;
                nsteps=1;
            }
            if(ncost<dist[nr][nc][nd][nsteps]){
                dist[nr][nc][nd][nsteps]=ncost;
                heapPush((State){ncost,nr,nc,nd,nsteps});
            }
        }
    }
    return -1;
}

int main(int argc,char *argv[]){
    FILE *f=fopen("input.txt","r");
    if(!f)return 1;
    while(height<MAX_HEIGHT && fgets(grid[height],MAX_WIDTH+2,f)){
        grid[height][strcspn(grid[height],"\n")]=0;
        if(height==0)width=strlen(grid[0]);
        if(strlen(grid[height])!=width){fclose(f);return 1;}
        height++;
    }
    fclose(f);
    if(height==0||width==0)return 1;
    printf("%d\n",solve(1,3));
    printf("%d\n",solve(4,10));
    return 0;
}
