
#import <Foundation/Foundation.h>
#import <pthread.h>

#define NUM_AMPLIFIERS 5
#define MAX_CODE_SIZE 4096

typedef struct {
    long long value;
    int hasValue;
    pthread_mutex_t mutex;
    pthread_cond_t canPut;
    pthread_cond_t canGet;
} Channel;

static inline void channelInit(Channel *ch) {
    ch->hasValue = 0;
    pthread_mutex_init(&ch->mutex, NULL);
    pthread_cond_init(&ch->canPut, NULL);
    pthread_cond_init(&ch->canGet, NULL);
}
static inline void channelDestroy(Channel *ch) {
    pthread_mutex_destroy(&ch->mutex);
    pthread_cond_destroy(&ch->canPut);
    pthread_cond_destroy(&ch->canGet);
}
static inline void channelPut(Channel *ch, long long v) {
    pthread_mutex_lock(&ch->mutex);
    while (ch->hasValue) pthread_cond_wait(&ch->canPut, &ch->mutex);
    ch->value = v;
    ch->hasValue = 1;
    pthread_cond_signal(&ch->canGet);
    pthread_mutex_unlock(&ch->mutex);
}
static inline long long channelGet(Channel *ch) {
    pthread_mutex_lock(&ch->mutex);
    while (!ch->hasValue) pthread_cond_wait(&ch->canGet, &ch->mutex);
    long long v = ch->value;
    ch->hasValue = 0;
    pthread_cond_signal(&ch->canPut);
    pthread_mutex_unlock(&ch->mutex);
    return v;
}

typedef struct {
    long long code[MAX_CODE_SIZE];
    int ip;
    Channel *in;
    Channel *out;
    int halted;
} VMState;

static inline long long getParam(VMState *vm, int off, int mode) {
    long long val = vm->code[vm->ip + off];
    return mode ? val : vm->code[val];
}
static void *vmThread(void *arg) {
    VMState *vm = arg;
    while (!vm->halted) {
        long long instr = vm->code[vm->ip];
        int opcode = instr % 100;
        int m1 = (instr / 100) % 10;
        int m2 = (instr / 1000) % 10;
        long long p1, p2, addr;
        switch (opcode) {
            case 1: p1 = getParam(vm,1,m1); p2 = getParam(vm,2,m2); addr = vm->code[vm->ip+3]; vm->code[addr] = p1+p2; vm->ip+=4; break;
            case 2: p1 = getParam(vm,1,m1); p2 = getParam(vm,2,m2); addr = vm->code[vm->ip+3]; vm->code[addr] = p1*p2; vm->ip+=4; break;
            case 3: addr = vm->code[vm->ip+1]; vm->code[addr] = channelGet(vm->in); vm->ip+=2; break;
            case 4: p1 = getParam(vm,1,m1); channelPut(vm->out,p1); vm->ip+=2; break;
            case 5: p1 = getParam(vm,1,m1); p2 = getParam(vm,2,m2); vm->ip = p1? p2 : vm->ip+3; break;
            case 6: p1 = getParam(vm,1,m1); p2 = getParam(vm,2,m2); vm->ip = !p1? p2 : vm->ip+3; break;
            case 7: p1 = getParam(vm,1,m1); p2 = getParam(vm,2,m2); addr = vm->code[vm->ip+3]; vm->code[addr] = p1<p2; vm->ip+=4; break;
            case 8: p1 = getParam(vm,1,m1); p2 = getParam(vm,2,m2); addr = vm->code[vm->ip+3]; vm->code[addr] = p1==p2; vm->ip+=4; break;
            case 99: vm->halted = 1; return NULL;
            default: vm->halted = 1; return NULL;
        }
    }
    return NULL;
}
static void swapInt(int *a,int *b){int t=*a;*a=*b;*b=t;}
static int nextPerm(int *a,int n){
    int i=n-2;
    while(i>=0 && a[i]>=a[i+1]) i--;
    if(i<0) return 0;
    int j=n-1;
    while(a[j]<=a[i]) j--;
    swapInt(&a[i],&a[j]);
    int l=i+1,r=n-1;
    while(l<r){swapInt(&a[l],&a[r]);l++;r--;}
    return 1;
}
static long long runFeedback(int *phase,long long *code,int size){
    Channel ch[NUM_AMPLIFIERS];
    VMState vm[NUM_AMPLIFIERS];
    pthread_t th[NUM_AMPLIFIERS];
    for(int i=0;i<NUM_AMPLIFIERS;i++){
        channelInit(&ch[i]);
        memcpy(vm[i].code,code,size*sizeof(long long));
        memset(vm[i].code+size,0,(MAX_CODE_SIZE-size)*sizeof(long long));
        vm[i].ip=0; vm[i].halted=0;
        vm[i].in=&ch[i];
        vm[i].out=&ch[(i+1)%NUM_AMPLIFIERS];
    }
    for(int i=0;i<NUM_AMPLIFIERS;i++){
        channelPut(vm[i].in,phase[i]);
        pthread_create(&th[i],NULL,vmThread,&vm[i]);
    }
    channelPut(vm[0].in,0);
    for(int i=0;i<NUM_AMPLIFIERS;i++) pthread_join(th[i],NULL);
    long long result = ch[0].value;
    for(int i=0;i<NUM_AMPLIFIERS;i++) channelDestroy(&ch[i]);
    return result;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        if (!content) { fprintf(stderr,"cannot read input.txt\n"); return 1; }
        NSArray *parts = [content componentsSeparatedByString:@","];
        long long code[MAX_CODE_SIZE];
        int codeSize = 0;
        for (NSString *p in parts) {
            if (codeSize>=MAX_CODE_SIZE) break;
            long long v = [p longLongValue];
            code[codeSize++] = v;
        }
        int phase[NUM_AMPLIFIERS] = {5,6,7,8,9};
        long long max = LLONG_MIN;
        do{
            long long out = runFeedback(phase,code,codeSize);
            if(out>max) max=out;
        }while(nextPerm(phase,NUM_AMPLIFIERS));
        printf("%lld\n",max);
    }
    return 0;
}
