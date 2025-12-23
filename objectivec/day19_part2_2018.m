
#import <Foundation/Foundation.h>

#define MAX_PROGRAM_LENGTH 1024
#define MAX_REGISTERS 6
typedef int (*operation)(int r[], int a, int b);

int addr(int r[], int a, int b){ return r[a]+r[b]; }
int addi(int r[], int a, int b){ return r[a]+b; }
int mulr(int r[], int a, int b){ return r[a]*r[b]; }
int muli(int r[], int a, int b){ return r[a]*b; }
int banr(int r[], int a, int b){ return r[a]&r[b]; }
int bani(int r[], int a, int b){ return r[a]&b; }
int borr(int r[], int a, int b){ return r[a]|r[b]; }
int bori(int r[], int a, int b){ return r[a]|b; }
int setr(int r[], int a, int b){ return r[a]; }
int seti(int r[], int a, int b){ return a; }
int gtir(int r[], int a, int b){ return a>r[b]?1:0; }
int gtri(int r[], int a, int b){ return r[a]>b?1:0; }
int gtrr(int r[], int a, int b){ return r[a]>r[b]?1:0; }
int eqir(int r[], int a, int b){ return a==r[b]?1:0; }
int eqri(int r[], int a, int b){ return r[a]==b?1:0; }
int eqrr(int r[], int a, int b){ return r[a]==r[b]?1:0; }

operation ops[] = {addr,addi,mulr,muli,banr,bani,borr,bori,setr,seti,gtir,gtri,gtrr,eqir,eqri,eqrr};
char *opcodes[] = {"addr","addi","mulr","muli","banr","bani","borr","bori","setr","seti","gtir","gtri","gtrr","eqir","eqri","eqrr"};

typedef struct{ operation op; int a,b,c; } Instruction;

int findOpcodeIndex(char *opcode){
    for(int i=0;i<16;i++) if(strcmp(opcodes[i],opcode)==0) return i;
    return -1;
}

void runProgram(int ipReg, Instruction prog[], int len, int regs[], int maxCycles){
    int ip=0, cycles=0;
    while(ip>=0 && ip<len){
        regs[ipReg]=ip;
        Instruction inst=prog[ip];
        regs[inst.c]=inst.op(regs,inst.a,inst.b);
        ip=regs[ipReg]+1;
        if(maxCycles>0 && ++cycles>=maxCycles) break;
    }
}

int maxVal(int *s, int len){
    int m=s[0];
    for(int i=1;i<len;i++) if(s[i]>m) m=s[i];
    return m;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        if(!data){ fprintf(stderr,"Failed to read input.txt\n"); return 1; }
        NSArray *lines = [data componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        int ipReg=0;
        Instruction prog[MAX_PROGRAM_LENGTH];
        int progLen=0;
        for(NSString *ln in lines){
            if([ln length]==0) continue;
            if([ln hasPrefix:@"#ip"]){
                sscanf([ln UTF8String], "#ip %d", &ipReg);
                continue;
            }
            char op[6];
            int a,b,c;
            sscanf([ln UTF8String], "%5s %d %d %d", op, &a, &b, &c);
            int idx=findOpcodeIndex(op);
            if(idx<0){ fprintf(stderr,"Unknown opcode %s\n",op); return 1; }
            prog[progLen++] = (Instruction){ops[idx],a,b,c};
        }
        int regs[MAX_REGISTERS] = {1,0,0,0,0,0};
        runProgram(ipReg, prog, progLen, regs, 1000);
        int n=maxVal(regs, MAX_REGISTERS);
        long long sum=0;
        for(int i=1;i<=n;i++) if(n%i==0) sum+=i;
        printf("%lld\n",sum);
    }
    return 0;
}
