
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MEM_SIZE 8192
#define MAP_DIM 101
#define MAP_OFFSET (MAP_DIM/2)
#define MAX_QUEUE (MAP_DIM*MAP_DIM*2)
#define UNKNOWN -1
#define WALL 0
#define FLOOR 1
#define OXYGEN 2
#define START 3
#define RUN_OK 0
#define RUN_INPUT_NEEDED 1
#define RUN_OUTPUT_READY 2
#define RUN_HALTED 3
#define NORTH 1
#define SOUTH 2
#define WEST 3
#define EAST 4

typedef struct{long long mem[MEM_SIZE]; long long ip; long long relative_base; int halted; long long input_val; long long output_val;}IntcodeState;
typedef struct{int x,y,dist;IntcodeState intcode;}BFSState;
typedef struct{BFSState items[MAX_QUEUE];int head;int tail;int count;}Queue;

int map[MAP_DIM][MAP_DIM];
int min_dist[MAP_DIM][MAP_DIM];
Queue bfs_queue;
int oxygen_x=-1,oxygen_y=-1;
int final_distance=-1;

void init_queue(){bfs_queue.head=bfs_queue.tail=bfs_queue.count=0;}
int is_queue_empty(){return bfs_queue.count==0;}
int is_queue_full(){return bfs_queue.count==MAX_QUEUE;}
void enqueue(BFSState s){bfs_queue.items[bfs_queue.tail++%MAX_QUEUE]=s;bfs_queue.count++;}
BFSState dequeue(){BFSState s=bfs_queue.items[bfs_queue.head++%MAX_QUEUE];bfs_queue.count--;return s;}

long long get_param_val(IntcodeState *s,int m,long long p){if(m==0)return s->mem[p];if(m==1)return p;return s->mem[s->relative_base+p];}
long long get_param_addr(IntcodeState *s,int m,long long p){if(m==0)return p;return s->relative_base+p;}

int run_intcode(IntcodeState *s){
    while(!s->halted&&s->ip>=0&&s->ip< MEM_SIZE){
        long long inst=s->mem[s->ip];
        int op=inst%100;
        int m1=(inst/100)%10,m2=(inst/1000)%10,m3=(inst/10000)%10;
        long long p1,p2,p3,addr;
        switch(op){
            case 1: p1=get_param_val(s,m1,s->mem[s->ip+1]);p2=get_param_val(s,m2,s->mem[s->ip+2]);addr=get_param_addr(s,m3,s->mem[s->ip+3]);s->mem[addr]=p1+p2;s->ip+=4;break;
            case 2: p1=get_param_val(s,m1,s->mem[s->ip+1]);p2=get_param_val(s,m2,s->mem[s->ip+2]);addr=get_param_addr(s,m3,s->mem[s->ip+3]);s->mem[addr]=p1*p2;s->ip+=4;break;
            case 3: addr=get_param_addr(s,m1,s->mem[s->ip+1]);s->ip+=2;return RUN_INPUT_NEEDED;
            case 4: p1=get_param_val(s,m1,s->mem[s->ip+1]);s->output_val=p1;s->ip+=2;return RUN_OUTPUT_READY;
            case 5: p1=get_param_val(s,m1,s->mem[s->ip+1]);p2=get_param_val(s,m2,s->mem[s->ip+2]);s->ip=(p1!=0?p2:s->ip+3);break;
            case 6: p1=get_param_val(s,m1,s->mem[s->ip+1]);p2=get_param_val(s,m2,s->mem[s->ip+2]);s->ip=(p1==0?p2:s->ip+3);break;
            case 7: p1=get_param_val(s,m1,s->mem[s->ip+1]);p2=get_param_val(s,m2,s->mem[s->ip+2]);addr=get_param_addr(s,m3,s->mem[s->ip+3]);s->mem[addr]=(p1<p2?1:0);s->ip+=4;break;
            case 8: p1=get_param_val(s,m1,s->mem[s->ip+1]);p2=get_param_val(s,m2,s->mem[s->ip+2]);addr=get_param_addr(s,m3,s->mem[s->ip+3]);s->mem[addr]=(p1==p2?1:0);s->ip+=4;break;
            case 9: p1=get_param_val(s,m1,s->mem[s->ip+1]);s->relative_base+=p1;s->ip+=2;break;
            case 99: s->halted=1;return RUN_HALTED;
            default:return RUN_HALTED;
        }
    }
    return RUN_HALTED;
}

void provide_input(IntcodeState *s,long long in){long long inst=s->mem[s->ip-2];int m1=(inst/100)%10;long long addr=get_param_addr(s,m1,s->mem[s->ip-1]);s->mem[addr]=in;}

void init_map(){for(int i=0;i<MAP_DIM;i++)for(int j=0;j<MAP_DIM;j++){map[i][j]=UNKNOWN;min_dist[i][j]=INT_MAX;}}

void explore(){
    init_queue();init_map();
    IntcodeState init_state={0};
    FILE *f=fopen("input.txt","r");if(!f){perror("input.txt");exit(1);}
    long long val;int idx=0;while(fscanf(f,"%lld,",&val)==1&&idx<MEM_SIZE){init_state.mem[idx++]=val;}fclose(f);
    int sx=MAP_OFFSET,sy=MAP_OFFSET;
    map[sy][sx]=FLOOR;min_dist[sy][sx]=0;
    enqueue((BFSState){sx,sy,0,init_state});
    int dx[4]={0,0,-1,1},dy[4]={-1,1,0,0};
    while(!is_queue_empty()){
        BFSState cur=dequeue();
        for(int m=1;m<=4;m++){
            int nx=cur.x+dx[m-1],ny=cur.y+dy[m-1];
            if(nx<0||nx>=MAP_DIM||ny<0||ny>=MAP_DIM)continue;
            IntcodeState ns=cur.intcode;
            int r=run_intcode(&ns);
            if(r==RUN_INPUT_NEEDED){provide_input(&ns,m);r=run_intcode(&ns);}
            if(r!=RUN_OUTPUT_READY)continue;
            int code=ns.output_val;
            int nd=cur.dist+1;
            if(code==WALL)continue;
            if(nd<min_dist[ny][nx]){
                map[ny][nx]=(code==OXYGEN?OXYGEN:FLOOR);
                min_dist[ny][nx]=nd;
                if(code==OXYGEN){printf("%d\n",nd);return;}
                enqueue((BFSState){nx,ny,nd,ns});
            }
        }
    }
    if(final_distance==-1){fprintf(stderr,"Oxygen not found\n");exit(1);}
}

int main(){explore();return 0;}
