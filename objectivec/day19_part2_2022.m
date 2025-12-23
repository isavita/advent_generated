
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_BLUEPRINTS 100
#define INITIAL_QUEUE_CAPACITY 1000000
#define HASH_TABLE_SIZE 19999999

typedef struct {
    int id;
    int ore_cost;
    int clay_ore_cost;
    int obsidian_ore_cost;
    int obsidian_clay_cost;
    int geode_ore_cost;
    int geode_obsidian_cost;
    int max_ore_needed;
} Blueprint;

typedef struct {
    short ore;
    short clay;
    short obsidian;
    short ore_robots;
    short clay_robots;
    short obsidian_robots;
    short geode_robots;
    short time_left;
    int geode;
} State;

typedef struct {
    State* data;
    size_t capacity;
    size_t size;
    size_t front;
    size_t rear;
} Queue;

static Queue* create_queue(size_t c){
    Queue* q=malloc(sizeof(Queue));
    q->data=malloc(c*sizeof(State));
    q->capacity=c;
    q->size=0;
    q->front=0;
    q->rear=0;
    return q;
}
static void resize_queue(Queue* q){
    size_t n=q->capacity*2;
    State* nd=malloc(n*sizeof(State));
    for(size_t i=0;i<q->size;i++) nd[i]=q->data[(q->front+i)%q->capacity];
    free(q->data);
    q->data=nd;
    q->capacity=n;
    q->front=0;
    q->rear=q->size;
}
static void enqueue(Queue* q,State s){
    if(q->size==q->capacity) resize_queue(q);
    q->data[q->rear]=s;
    q->rear=(q->rear+1)%q->capacity;
    q->size++;
}
static State dequeue(Queue* q){
    State s=q->data[q->front];
    q->front=(q->front+1)%q->capacity;
    q->size--;
    return s;
}
static void destroy_queue(Queue* q){
    free(q->data);
    free(q);
}

typedef struct {
    State* table;
    bool* occupied;
    size_t capacity;
} VisitedSet;

static VisitedSet* create_visited_set(size_t c){
    VisitedSet* v=malloc(sizeof(VisitedSet));
    v->capacity=c;
    v->table=calloc(c,sizeof(State));
    v->occupied=calloc(c,sizeof(bool));
    return v;
}
static size_t hash_state(const State* s,size_t cap){
    size_t h=5381;
    h=((h<<5)+h)+s->ore;
    h=((h<<5)+h)+s->clay;
    h=((h<<5)+h)+s->obsidian;
    h=((h<<5)+h)+s->geode;
    h=((h<<5)+h)+s->ore_robots;
    h=((h<<5)+h)+s->clay_robots;
    h=((h<<5)+h)+s->obsidian_robots;
    h=((h<<5)+h)+s->geode_robots;
    h=((h<<5)+h)+s->time_left;
    return h%cap;
}
static bool insert_visited(VisitedSet* v,State s){
    size_t i=hash_state(&s,v->capacity),st=i;
    while(v->occupied[i]){
        if(memcmp(&v->table[i],&s,sizeof(State))==0) return false;
        i=(i+1)%v->capacity;
        if(i==st) exit(1);
    }
    v->table[i]=s;
    v->occupied[i]=true;
    return true;
}
static void destroy_visited_set(VisitedSet* v){
    free(v->table);
    free(v->occupied);
    free(v);
}
static inline int max(int a,int b){return a>b?a:b;}
static inline int max4(int a,int b,int c,int d){return max(max(a,b),max(c,d));}

static int max_geode(const Blueprint* b,State init){
    int best=0;
    Queue* q=create_queue(INITIAL_QUEUE_CAPACITY);
    VisitedSet* vis=create_visited_set(HASH_TABLE_SIZE);
    enqueue(q,init);
    while(q->size){
        State s=dequeue(q);
        if(s.geode>best) best=s.geode;
        if(s.time_left==0) continue;
        State cs=s;
        if(cs.ore_robots>=b->max_ore_needed) cs.ore_robots=b->max_ore_needed;
        if(cs.clay_robots>=b->obsidian_clay_cost) cs.clay_robots=b->obsidian_clay_cost;
        if(cs.obsidian_robots>=b->geode_obsidian_cost) cs.obsidian_robots=b->geode_obsidian_cost;
        int max_ore_useful=s.time_left*b->max_ore_needed - s.ore_robots*(s.time_left-1);
        if(cs.ore>max_ore_useful) cs.ore=max_ore_useful;
        int max_clay_useful=s.time_left*b->obsidian_clay_cost - s.clay_robots*(s.time_left-1);
        if(cs.clay>max_clay_useful) cs.clay=max_clay_useful;
        int max_obs_useful=s.time_left*b->geode_obsidian_cost - s.obsidian_robots*(s.time_left-1);
        if(cs.obsidian>max_obs_useful) cs.obsidian=max_obs_useful;
        if(!insert_visited(vis,cs)) continue;
        State nxt=s;
        nxt.time_left--;
        nxt.ore+=s.ore_robots;
        nxt.clay+=s.clay_robots;
        nxt.obsidian+=s.obsidian_robots;
        nxt.geode+=s.geode_robots;
        enqueue(q,nxt);
        if(s.ore>=b->ore_cost){
            State nb=s;
            nb.time_left--;
            nb.ore-=b->ore_cost;
            nb.ore+=s.ore_robots;
            nb.clay+=s.clay_robots;
            nb.obsidian+=s.obsidian_robots;
            nb.geode+=s.geode_robots;
            nb.ore_robots++;
            enqueue(q,nb);
        }
        if(s.ore>=b->clay_ore_cost){
            State nb=s;
            nb.time_left--;
            nb.ore-=b->clay_ore_cost;
            nb.ore+=s.ore_robots;
            nb.clay+=s.clay_robots;
            nb.obsidian+=s.obsidian_robots;
            nb.geode+=s.geode_robots;
            nb.clay_robots++;
            enqueue(q,nb);
        }
        if(s.ore>=b->obsidian_ore_cost && s.clay>=b->obsidian_clay_cost){
            State nb=s;
            nb.time_left--;
            nb.ore-=b->obsidian_ore_cost;
            nb.clay-=b->obsidian_clay_cost;
            nb.ore+=s.ore_robots;
            nb.clay+=s.clay_robots;
            nb.obsidian+=s.obsidian_robots;
            nb.geode+=s.geode_robots;
            nb.obsidian_robots++;
            enqueue(q,nb);
        }
        if(s.ore>=b->geode_ore_cost && s.obsidian>=b->geode_obsidian_cost){
            State nb=s;
            nb.time_left--;
            nb.ore-=b->geode_ore_cost;
            nb.obsidian-=b->geode_obsidian_cost;
            nb.ore+=s.ore_robots;
            nb.clay+=s.clay_robots;
            nb.obsidian+=s.obsidian_robots;
            nb.geode+=s.geode_robots;
            nb.geode_robots++;
            enqueue(q,nb);
        }
    }
    destroy_queue(q);
    destroy_visited_set(vis);
    return best;
}

int main(int argc, const char * argv[]) {
    FILE* f=fopen("input.txt","r");
    if(!f) return 1;
    Blueprint bps[MAX_BLUEPRINTS];
    int cnt=0;
    char line[256];
    while(cnt<MAX_BLUEPRINTS && fgets(line,sizeof(line),f)){
        Blueprint* b=&bps[cnt];
        if(sscanf(line,
            "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian.",
            &b->id,&b->ore_cost,&b->clay_ore_cost,&b->obsidian_ore_cost,&b->obsidian_clay_cost,&b->geode_ore_cost,&b->geode_obsidian_cost)==7){
            b->max_ore_needed=max4(b->ore_cost,b->clay_ore_cost,b->obsidian_ore_cost,b->geode_ore_cost);
            cnt++;
        }
    }
    fclose(f);
    if(cnt==0) return 1;
    long long prod=1;
    int limit=cnt<3?cnt:3;
    for(int i=0;i<limit;i++){
        State init={0,0,0,1,0,0,0,32,0};
        int r=max_geode(&bps[i],init);
        if(r==0){prod=0;break;}
        prod*=r;
    }
    printf("%lld\n",prod);
    return 0;
}
