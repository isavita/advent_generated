
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef long long ll;
#define INITIAL_MEM_CAPACITY 4096
#define NUM_COMPUTERS 50
#define QUEUE_CAPACITY 1024

typedef struct {
    ll buffer[QUEUE_CAPACITY];
    int front;
    int rear;
    int size;
} Queue;

void init_queue(Queue *q) { q->front = 0; q->rear = -1; q->size = 0; }
bool is_empty_queue(Queue *q) { return q->size == 0; }
bool enqueue(Queue *q, ll v) {
    if (q->size >= QUEUE_CAPACITY) return false;
    q->rear = (q->rear + 1) % QUEUE_CAPACITY;
    q->buffer[q->rear] = v;
    q->size++;
    return true;
}
bool dequeue(Queue *q, ll *v) {
    if (is_empty_queue(q)) return false;
    *v = q->buffer[q->front];
    q->front = (q->front + 1) % QUEUE_CAPACITY;
    q->size--;
    return true;
}

typedef struct {
    ll *memory;
    size_t mem_capacity;
    ll ip;
    ll relative_base;
    Queue inputs;
    Queue outputs;
    bool halted;
    bool idle;
} IntcodeComputer;

void ensure_mem_capacity(IntcodeComputer *c, size_t idx) {
    if (idx >= c->mem_capacity) {
        size_t old = c->mem_capacity;
        size_t newcap = old;
        while (idx >= newcap) newcap <<= 1;
        ll *newmem = realloc(c->memory, newcap * sizeof(ll));
        if (!newmem) { perror("realloc"); exit(EXIT_FAILURE); }
        memset(newmem + old, 0, (newcap - old) * sizeof(ll));
        c->memory = newmem;
        c->mem_capacity = newcap;
    }
}
ll mem_get(IntcodeComputer *c, ll addr) {
    if (addr < 0) { fprintf(stderr,"neg addr %lld\n",addr); exit(EXIT_FAILURE); }
    ensure_mem_capacity(c, addr);
    return c->memory[addr];
}
void mem_set(IntcodeComputer *c, ll addr, ll val) {
    if (addr < 0) { fprintf(stderr,"neg write %lld\n",addr); exit(EXIT_FAILURE); }
    ensure_mem_capacity(c, addr);
    c->memory[addr] = val;
}
ll param_addr(IntcodeComputer *c, int mode, ll offs) {
    ll val = mem_get(c, c->ip + offs);
    if (mode == 0) return val;
    if (mode == 2) return c->relative_base + val;
    fprintf(stderr,"bad addr mode %d\n",mode); exit(EXIT_FAILURE);
}
ll param(IntcodeComputer *c, int mode, ll offs) {
    ll val = mem_get(c, c->ip + offs);
    if (mode == 0) return mem_get(c, val);
    if (mode == 1) return val;
    if (mode == 2) return mem_get(c, c->relative_base + val);
    fprintf(stderr,"bad param mode %d\n",mode); exit(EXIT_FAILURE);
}
void run_computer(IntcodeComputer *c) {
    c->idle = false;
    while (1) {
        ll instr = mem_get(c, c->ip);
        int op = instr % 100;
        int m[3] = {(int)((instr/100)%10),(int)((instr/1000)%10),(int)((instr/10000)%10)};
        if (op == 99) { c->halted = true; break; }
        ll p1,p2,addr;
        switch(op) {
            case 1: case 2: case 7: case 8:
                p1 = param(c,m[0],1); p2 = param(c,m[1],2);
                addr = param_addr(c,m[2],3);
                ll r;
                if(op==1) r=p1+p2; else if(op==2) r=p1*p2;
                else if(op==7) r=(p1<p2)?1:0; else r=(p1==p2)?1:0;
                mem_set(c,addr,r); c->ip+=4; break;
            case 3:
                if (!dequeue(&c->inputs,&p1)) { mem_set(c,param_addr(c,m[0],1),-1); c->idle=true; c->ip+=2; return; }
                if (p1==-1) { mem_set(c,param_addr(c,m[0],1),-1); c->idle=true; c->ip+=2; return; }
                mem_set(c,param_addr(c,m[0],1),p1); c->idle=false; c->ip+=2; break;
            case 4:
                p1 = param(c,m[0],1);
                if (!enqueue(&c->outputs,p1)) { fprintf(stderr,"out full\n"); exit(EXIT_FAILURE); }
                c->ip+=2; c->idle=false;
                if (c->outputs.size>=3) return;
                break;
            case 5: case 6:
                p1 = param(c,m[0],1); p2 = param(c,m[1],2);
                if ((op==5 && p1!=0) || (op==6 && p1==0)) c->ip=p2; else c->ip+=3;
                break;
            case 9:
                p1 = param(c,m[0],1); c->relative_base+=p1; c->ip+=2; break;
            default:
                fprintf(stderr,"bad op %d\n",op); c->halted=true; return;
        }
    }
}

int main(int argc, const char *argv[]) {
    FILE *fp = fopen("input.txt","r");
    if (!fp) { perror("open"); return EXIT_FAILURE; }
    ll *prog=NULL; size_t ps=0,pc=0,val; char ch;
    while (fscanf(fp,"%lld",&val)==1) {
        if (ps>=pc) { pc=pc?pc*2:128; ll *np=realloc(prog,pc*sizeof(ll)); if (!np) { perror("realloc"); free(prog); fclose(fp); return EXIT_FAILURE; } prog=np; }
        prog[ps++]=val;
        if (fscanf(fp,"%c",&ch)!=1 || (ch!=',' && ch!='\n' && !feof(fp))) { if (!feof(fp)) fprintf(stderr,"bad char\n"); }
        if (feof(fp)) break;
    }
    fclose(fp);
    if (!prog) { fprintf(stderr,"no prog\n"); return EXIT_FAILURE; }

    IntcodeComputer comps[NUM_COMPUTERS];
    Queue pktq[NUM_COMPUTERS];
    for (int i=0;i<NUM_COMPUTERS;i++) {
        comps[i].mem_capacity = ps>INITIAL_MEM_CAPACITY?ps*2:INITIAL_MEM_CAPACITY;
        comps[i].memory = malloc(comps[i].mem_capacity*sizeof(ll));
        if (!comps[i].memory) { perror("mem"); return EXIT_FAILURE; }
        memset(comps[i].memory,0,comps[i].mem_capacity*sizeof(ll));
        memcpy(comps[i].memory,prog,ps*sizeof(ll));
        comps[i].ip=0; comps[i].relative_base=0;
        init_queue(&comps[i].inputs); init_queue(&comps[i].outputs);
        comps[i].halted=false; comps[i].idle=false;
        init_queue(&pktq[i]);
        enqueue(&comps[i].inputs,(ll)i);
    }
    free(prog);

    ll natx=-1,naty=-1,prevnaty=-2; bool nathas=false;
    while (1) {
        bool netidle=true;
        for (int i=0;i<NUM_COMPUTERS;i++) {
            if (comps[i].halted) continue;
            if (is_empty_queue(&pktq[i])) {
                if (is_empty_queue(&comps[i].inputs)) enqueue(&comps[i].inputs,-1);
            } else {
                netidle=false;
                ll x,y; dequeue(&pktq[i],&x); dequeue(&pktq[i],&y);
                enqueue(&comps[i].inputs,x); enqueue(&comps[i].inputs,y);
            }
            run_computer(&comps[i]);
            if (!comps[i].idle) netidle=false;
            while (comps[i].outputs.size>=3) {
                netidle=false;
                ll d,x,y;
                dequeue(&comps[i].outputs,&d);
                dequeue(&comps[i].outputs,&x);
                dequeue(&comps[i].outputs,&y);
                if (d==255) { natx=x; naty=y; nathas=true; }
                else if (d>=0 && d<NUM_COMPUTERS) { enqueue(&pktq[d],x); enqueue(&pktq[d],y); }
            }
        }
        bool queues_empty=true, comps_idle=true;
        for (int i=0;i<NUM_COMPUTERS;i++) { if (!is_empty_queue(&pktq[i])) { queues_empty=false; break; } }
        for (int i=0;i<NUM_COMPUTERS;i++) { if (!comps[i].halted && !comps[i].idle) { comps_idle=false; break; } }
        if (netidle && queues_empty && comps_idle) {
            if (nathas) {
                if (naty==prevnaty) { printf("%lld\n",naty); goto cleanup; }
                prevnaty=naty;
                enqueue(&pktq[0],natx); enqueue(&pktq[0],naty);
                comps[0].idle=false; nathas=false;
            } else {
                bool allhalt=true;
                for (int i=0;i<NUM_COMPUTERS;i++) if (!comps[i].halted) { allhalt=false; break; }
                if (allhalt) { fprintf(stderr,"stuck\n"); goto cleanup; }
            }
        }
    }

cleanup:
    for (int i=0;i<NUM_COMPUTERS;i++) free(comps[i].memory);
    return EXIT_SUCCESS;
}
