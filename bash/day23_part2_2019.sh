#!/usr/bin/env bash
# Compile the provided C program and run it on input.txt
# The C program already reads from input.txt and prints the answer.

cat > nat.c <<'EOF'
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define INITIAL_MEM_CAPACITY 4096
#define NUM_COMPUTERS 50
#define QUEUE_CAPACITY 1024

typedef long long ll;

typedef struct {
    ll buffer[QUEUE_CAPACITY];
    int front;
    int rear;
    int size;
} Queue;

void init_queue(Queue *q) { q->front = 0; q->rear = -1; q->size = 0; }
bool is_empty_queue(Queue *q) { return q->size == 0; }
bool enqueue(Queue *q, ll v) { if (q->size>=QUEUE_CAPACITY) return false; q->rear=(q->rear+1)%QUEUE_CAPACITY; q->buffer[q->rear]=v; q->size++; return true; }
bool dequeue(Queue *q, ll *v){ if(is_empty_queue(q)) return false; *v=q->buffer[q->front]; q->front=(q->front+1)%QUEUE_CAPACITY; q->size--; return true; }

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

void ensure_mem_capacity(IntcodeComputer *c, size_t idx){
    if(idx>=c->mem_capacity){
        size_t old=c->mem_capacity, new=old;
        while(idx>=new) new*=2;
        ll *tmp=realloc(c->memory,new*sizeof(ll));
        if(!tmp){ perror("realloc"); exit(1); }
        memset(tmp+old,0,(new-old)*sizeof(ll));
        c->memory=tmp; c->mem_capacity=new;
    }
}
ll mem_get(IntcodeComputer *c,ll a){ if(a<0){fprintf(stderr,"neg mem\n"); exit(1);} ensure_mem_capacity(c,a); return c->memory[a]; }
void mem_set(IntcodeComputer *c,ll a,ll v){ if(a<0){fprintf(stderr,"neg mem\n"); exit(1);} ensure_mem_capacity(c,a); c->memory[a]=v; }

ll get_param_addr(IntcodeComputer *c,int mode,ll off){
    ll addr=c->ip+off; ll val=mem_get(c,addr);
    if(mode==0) return val;
    if(mode==2) return c->relative_base+val;
    fprintf(stderr,"bad mode\n"); exit(1);
}
ll get_param(IntcodeComputer *c,int mode,ll off){
    ll addr=c->ip+off; ll val=mem_get(c,addr);
    if(mode==0) return mem_get(c,val);
    if(mode==1) return val;
    if(mode==2) return mem_get(c,c->relative_base+val);
    fprintf(stderr,"bad mode\n"); exit(1);
}

void run_computer(IntcodeComputer *c){
    c->idle=false;
    while(1){
        ll instr=mem_get(c,c->ip);
        int opcode=instr%100;
        int m[3]={ (instr/100)%10,(instr/1000)%10,(instr/10000)%10 };
        if(opcode==99){ c->halted=true; break; }
        ll p1,p2,addr;
        switch(opcode){
            case 1: case 2: case 7: case 8:
                p1=get_param(c,m[0],1); p2=get_param(c,m[1],2);
                addr=get_param_addr(c,m[2],3);
                ll r;
                if(opcode==1) r=p1+p2;
                else if(opcode==2) r=p1*p2;
                else if(opcode==7) r=p1<p2?1:0;
                else r=p1==p2?1:0;
                mem_set(c,addr,r); c->ip+=4; break;
            case 3:{
                addr=get_param_addr(c,m[0],1);
                ll val;
                if(!dequeue(&c->inputs,&val)){
                    mem_set(c,addr,-1); c->idle=true; c->ip+=2; return;
                }
                if(val==-1) c->idle=true; else c->idle=false;
                mem_set(c,addr,val); c->ip+=2;
                if(val==-1 && is_empty_queue(&c->inputs)) return;
                break;
            }
            case 4:{
                p1=get_param(c,m[0],1);
                enqueue(&c->outputs,p1); c->ip+=2; c->idle=false;
                if(c->outputs.size>=3) return;
                break;
            }
            case 5: case 6:{
                p1=get_param(c,m[0],1); p2=get_param(c,m[1],2);
                if((opcode==5&&p1!=0)||(opcode==6&&p1==0)) c->ip=p2; else c->ip+=3;
                break;
            }
            case 9:{
                p1=get_param(c,m[0],1); c->relative_base+=p1; c->ip+=2; break;
            }
            default: fprintf(stderr,"bad opcode\n"); c->halted=true; return;
        }
    }
}

int main(){
    FILE *f=fopen("input.txt","r");
    if(!f){ perror("input.txt"); return 1; }
    ll *prog=NULL; size_t sz=0,cap=0,val; char ch;
    while(fscanf(f,"%lld",&val)==1){
        if(sz>=cap){ cap=cap?cap*2:128; prog=realloc(prog,cap*sizeof(ll)); }
        prog[sz++]=val;
        if(fscanf(f,"%c",&ch)!=1||ch!=','&&ch!='\n') if(!feof(f)) fprintf(stderr,"bad char\n");
        if(feof(f)) break;
    }
    fclose(f);
    if(!prog){ fprintf(stderr,"no program\n"); return 1; }

    IntcodeComputer comp[NUM_COMPUTERS];
    Queue pkt[NUM_COMPUTERS];
    for(int i=0;i<NUM_COMPUTERS;i++){
        comp[i].mem_capacity=sz>INITIAL_MEM_CAPACITY?sz*2:INITIAL_MEM_CAPACITY;
        comp[i].memory=calloc(comp[i].mem_capacity,sizeof(ll));
        memcpy(comp[i].memory,prog,sz*sizeof(ll));
        comp[i].ip=0; comp[i].relative_base=0;
        init_queue(&comp[i].inputs); init_queue(&comp[i].outputs);
        comp[i].halted=false; comp[i].idle=false;
        init_queue(&pkt[i]);
        enqueue(&comp[i].inputs,(ll)i);
    }
    free(prog);
    ll natx=-1,naty=-1,prev=-2; bool nat=false;
    while(1){
        bool idle=true;
        for(int i=0;i<NUM_COMPUTERS;i++){
            if(comp[i].halted) continue;
            if(is_empty_queue(&pkt[i])){
                if(is_empty_queue(&comp[i].inputs)) enqueue(&comp[i].inputs,-1);
            }else{
                idle=false;
                ll x,y; dequeue(&pkt[i],&x); dequeue(&pkt[i],&y);
                enqueue(&comp[i].inputs,x); enqueue(&comp[i].inputs,y);
            }
            run_computer(&comp[i]);
            if(!comp[i].idle) idle=false;
            while(comp[i].outputs.size>=3){
                idle=false;
                ll d,x,y; dequeue(&comp[i].outputs,&d); dequeue(&comp[i].outputs,&x); dequeue(&comp[i].outputs,&y);
                if(d==255){ natx=x; naty=y; nat=true; }
                else if(d>=0&&d<NUM_COMPUTERS){ enqueue(&pkt[d],x); enqueue(&pkt[d],y); }
            }
        }
        bool allq=true; for(int i=0;i<NUM_COMPUTERS;i++) if(!is_empty_queue(&pkt[i])){ allq=false; break; }
        bool allidle=true; for(int i=0;i<NUM_COMPUTERS;i++) if(!comp[i].halted && !comp[i].idle){ allidle=false; break; }
        if(idle && allq && allidle){
            if(nat){
                if(naty==prev){ printf("%lld\n",naty); return 0; }
                prev=naty;
                enqueue(&pkt[0],natx); enqueue(&pkt[0],naty);
                comp[0].idle=false; nat=false;
            }else{
                bool allh=true; for(int i=0;i<NUM_COMPUTERS;i++) if(!comp[i].halted){ allh=false; break; }
                if(allh){ fprintf(stderr,"stuck\n"); return 1; }
            }
        }
    }
    return 0;
}
EOF

gcc nat.c -o nat
./nat
rm nat.c nat
exit 0
