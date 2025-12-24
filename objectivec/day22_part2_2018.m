#import <Foundation/Foundation.h>

#define GEOLOGIC_Y 16807LL
#define GEOLOGIC_X 48271LL
#define CAVE_MODULO 20183
#define TYPE_ROCKY 0
#define TYPE_WET 1
#define TYPE_NARROW 2
#define TOOL_NONE 1
#define TOOL_TORCH 2
#define TOOL_GEAR 4
#define MAX_TOOL_VALUE 4
#define INF INT_MAX
#define PADDING 100
#define HEAP_MAX_SIZE 4000000

typedef struct {
    int time,x,y,tool;
} State;

static int g_depth,g_targetX,g_targetY,g_boundX,g_boundY,g_heapSize=0;
static long long **gGeo;
static int **gEro,***gDist;
static State gHeap[HEAP_MAX_SIZE];

static void swap(State *a,State *b){State t=*a;*a=*b;*b=t;}
static void heapUp(int i){
    while(i>0){
        int p=(i-1)/2;
        if(gHeap[i].time<gHeap[p].time){swap(&gHeap[i],&gHeap[p]);i=p;}
        else break;
    }
}
static void heapDown(int i){
    while(1){
        int l=2*i+1,r=2*i+2,s=i;
        if(l<g_heapSize&&gHeap[l].time<gHeap[s].time)s=l;
        if(r<g_heapSize&&gHeap[r].time<gHeap[s].time)s=r;
        if(s!=i){swap(&gHeap[i],&gHeap[s]);i=s;}
        else break;
    }
}
static void push(State s){
    gHeap[g_heapSize]=s;
    heapUp(g_heapSize++);
}
static State pop(void){
    State top=gHeap[0];
    gHeap[0]=gHeap[--g_heapSize];
    heapDown(0);
    return top;
}

static long long geo(int x,int y){
    if(x<0||y<0||x>=g_boundX||y>=g_boundY)return -1;
    if(gGeo[y][x]!=-1)return gGeo[y][x];
    long long v;
    if(x==0&&y==0)v=0;
    else if(x==g_targetX&&y==g_targetY)v=0;
    else if(y==0)v=(long long)x*GEOLOGIC_Y;
    else if(x==0)v=(long long)y*GEOLOGIC_X;
    else v=gEro[y][x-1]*(long long)gEro[y-1][x];
    return gGeo[y][x]=v;
}
static int erosion(int x,int y){
    if(x<0||y<0||x>=g_boundX||y>=g_boundY)return -1;
    if(gEro[y][x]!=-1)return gEro[y][x];
    long long g=geo(x,y);
    return gEro[y][x]=(int)((g+g_depth)%CAVE_MODULO);
}
static int type(int x,int y){return erosion(x,y)%3;}
static int allowed(int t){
    switch(t){
        case TYPE_ROCKY:return TOOL_GEAR|TOOL_TORCH;
        case TYPE_WET:return TOOL_GEAR|TOOL_NONE;
        case TYPE_NARROW:return TOOL_TORCH|TOOL_NONE;
        default:return 0;
    }
}

int main(int argc,char *argv[]){
    @autoreleasepool {
        NSString *path=[NSString stringWithUTF8String:"input.txt"];
        NSString *txt=[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSScanner *sc=[NSScanner scannerWithString:txt];
        NSString *line;
        [sc scanUpToString:@"\n" intoString:&line];
        sscanf([line UTF8String],"depth: %d",&g_depth);
        [sc scanUpToString:@"," intoString:&line];
        sscanf([line UTF8String],"target: %d",&g_targetX);
        [sc scanString:@"," intoString:nil];
        [sc scanInt:&g_targetY];

        g_boundX=g_targetX+PADDING;
        g_boundY=g_targetY+PADDING;
        gGeo=malloc(g_boundY*sizeof(long long*));
        gEro=malloc(g_boundY*sizeof(int*));
        gDist=malloc(g_boundY*sizeof(int**));
        for(int y=0;y<g_boundY;y++){
            gGeo[y]=malloc(g_boundX*sizeof(long long));
            gEro[y]=malloc(g_boundX*sizeof(int));
            gDist[y]=malloc(g_boundX*sizeof(int*));
            for(int x=0;x<g_boundX;x++){
                gGeo[y][x]=-1;
                gEro[y][x]=-1;
                gDist[y][x]=malloc((MAX_TOOL_VALUE+1)*sizeof(int));
                for(int t=0;t<=MAX_TOOL_VALUE;t++)gDist[y][x][t]=INF;
            }
        }
        for(int y=0;y<g_boundY;y++)for(int x=0;x<g_boundX;x++)erosion(x,y);

        gDist[0][0][TOOL_TORCH]=0;
        push((State){0,0,0,TOOL_TORCH});
        int dx[]={0,0,1,-1},dy[]={1,-1,0,0},final=-1;
        while(g_heapSize){
            State c=pop();
            if(c.time>gDist[c.y][c.x][c.tool])continue;
            if(c.x==g_targetX&&c.y==g_targetY&&c.tool==TOOL_TORCH){final=c.time;break;}
            int here=type(c.x,c.y),allow=allowed(here);
            for(int nt=1;nt<=MAX_TOOL_VALUE;nt<<=1){
                if((nt&allow)&&nt!=c.tool){
                    int ntme=c.time+7;
                    if(ntme<gDist[c.y][c.x][nt]){
                        gDist[c.y][c.x][nt]=ntme;
                        push((State){ntme,c.x,c.y,nt});
                    }
                }
            }
            for(int i=0;i<4;i++){
                int nx=c.x+dx[i],ny=c.y+dy[i];
                if(nx<0||ny<0||nx>=g_boundX||ny>=g_boundY)continue;
                int ntyp=type(nx,ny),nallow=allowed(ntyp);
                if(c.tool&nallow){
                    int ntme=c.time+1;
                    if(ntme<gDist[ny][nx][c.tool]){
                        gDist[ny][nx][c.tool]=ntme;
                        push((State){ntme,nx,ny,c.tool});
                    }
                }
            }
        }
        printf("%d\n",final);
        for(int y=0;y<g_boundY;y++){
            for(int x=0;x<g_boundX;x++)free(gDist[y][x]);
            free(gGeo[y]);free(gEro[y]);free(gDist[y]);
        }
        free(gGeo);free(gEro);free(gDist);
    }
    return 0;
}