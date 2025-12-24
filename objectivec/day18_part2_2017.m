
#import <Foundation/Foundation.h>
#define MAX_INSTRUCTIONS 100
#define MAX_LINE_LEN 64
#define QUEUE_SIZE 100000
#define NUM_REGISTERS 26

typedef enum {SND,SET,ADD,MUL,MOD,RCV,JGZ} OpCode;
typedef struct {BOOL isReg; long long val;} Arg;
typedef struct {OpCode op; Arg a1; Arg a2;} Instr;
typedef struct {long long data[QUEUE_SIZE]; int head,tail,count;} Q;

static Instr instrs[MAX_INSTRUCTIONS];
static int instrCount = 0;
static long long regs[2][NUM_REGISTERS];
static int pc[2] = {0,0};
static BOOL waiting[2] = {NO,NO};
static BOOL terminated[2] = {NO,NO};
static Q q0,q1;
static long long sndCount = 0;

static inline void qInit(Q *q){q->head=q->tail=q->count=0;}
static inline BOOL qEmpty(Q *q){return q->count==0;}
static inline void qPush(Q *q,long long v){q->data[q->tail]=v;q->tail=(q->tail+1)%QUEUE_SIZE;q->count++;}
static inline long long qPop(Q *q){long long v=q->data[q->head];q->head=(q->head+1)%QUEUE_SIZE;q->count--;return v;}
static inline long long getVal(Arg a,int pid){return a.isReg?regs[pid][a.val]:a.val;}
static inline Arg parseArg(const char *s){
    Arg a; a.isReg=isalpha(s[0])&&s[1]==0; a.val=a.isReg?(s[0]-'a'):atoll(s); return a;
}

int main(int argc,const char *argv[]){
    @autoreleasepool{
        FILE *f=fopen("input.txt","r");
        if(!f)return 1;
        char line[MAX_LINE_LEN],c1[5],c2[20],c3[20];
        while(fgets(line,sizeof line,f)&&instrCount<MAX_INSTRUCTIONS){
            int n=sscanf(line,"%4s %19s %19s",c1,c2,c3);
            if(n<2)continue;
            Instr *i=&instrs[instrCount];
            if(!strcmp(c1,"snd"))i->op=SND;
            else if(!strcmp(c1,"set"))i->op=SET;
            else if(!strcmp(c1,"add"))i->op=ADD;
            else if(!strcmp(c1,"mul"))i->op=MUL;
            else if(!strcmp(c1,"mod"))i->op=MOD;
            else if(!strcmp(c1,"rcv"))i->op=RCV;
            else if(!strcmp(c1,"jgz"))i->op=JGZ;
            else continue;
            i->a1=parseArg(c2);
            i->a2=n==3?parseArg(c3):(Arg){NO,0};
            instrCount++;
        }
        fclose(f);
        memset(regs,0,sizeof regs);
        regs[1]['p'-'a']=1;
        qInit(&q0);qInit(&q1);
        while(!(terminated[0]&&terminated[1])&&!(waiting[0]&&waiting[1])){
            for(int pid=0;pid<2;pid++){
                if(terminated[pid]||waiting[pid])continue;
                while(pc[pid]>=0&&pc[pid]<instrCount){
                    Instr ins=instrs[pc[pid]];
                    int r=ins.a1.isReg?ins.a1.val:-1;
                    switch(ins.op){
                        case SND:
                            if(pid==0)qPush(&q1,getVal(ins.a1,0));
                            else{qPush(&q0,getVal(ins.a1,1));sndCount++;}
                            break;
                        case SET: if(r!=-1)regs[pid][r]=getVal(ins.a2,pid); break;
                        case ADD: if(r!=-1)regs[pid][r]+=getVal(ins.a2,pid); break;
                        case MUL: if(r!=-1)regs[pid][r]*=getVal(ins.a2,pid); break;
                        case MOD: if(r!=-1){long long v=getVal(ins.a2,pid);regs[pid][r]=v?regs[pid][r]%v:0;} break;
                        case RCV:{
                            Q *rq=pid?&q1:&q0;
                            if(qEmpty(rq)){waiting[pid]=YES;goto progEnd;}
                            if(r!=-1)regs[pid][r]=qPop(rq);
                            waiting[pid]=NO;
                        }break;
                        case JGZ:
                            if(getVal(ins.a1,pid)>0){pc[pid]+=getVal(ins.a2,pid);continue;}
                            break;
                    }
                    pc[pid]++;
                    if(ins.op==SND && waiting[1-pid])waiting[1-pid]=NO;
                }
                if(pc[pid]<0||pc[pid]>=instrCount)terminated[pid]=YES;
            progEnd:;}
        }
        printf("%lld\n",sndCount);
    }
    return 0;
}
