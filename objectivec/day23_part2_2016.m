#import <Foundation/Foundation.h>

#define MAX 1000

typedef struct {
    char op[4];
    int t1, v1, t2, v2;
} I;

static inline int reg(NSString *s) { return s.length==1 && [@"abcd" containsString:s]; }
static inline int idx(NSString *s) { return [s characterAtIndex:0] - 'a'; }

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSArray *lines = [txt componentsSeparatedByString:@"\n"];
        I prog[MAX];
        int n=0;
        for (NSString *l in lines) {
            NSArray *p = [l componentsSeparatedByString:@" "];
            if (p.count<2) continue;
            strcpy(prog[n].op, [p[0] UTF8String]);
            NSString *a1 = p[1];
            if (reg(a1)) { prog[n].t1=0; prog[n].v1=idx(a1); }
            else         { prog[n].t1=1; prog[n].v1=[a1 intValue]; }
            if (p.count>2) {
                NSString *a2 = p[2];
                if (reg(a2)) { prog[n].t2=0; prog[n].v2=idx(a2); }
                else         { prog[n].t2=1; prog[n].v2=[a2 intValue]; }
            }
            n++;
        }
        long r[4]={12,0,0,0};
        for (int pc=0;pc<n;pc++){
            I *in=&prog[pc];
            long x1 = in->t1? in->v1 : r[in->v1];
            if (!strcmp(in->op,"cpy")) r[in->v2]=x1;
            else if (!strcmp(in->op,"inc")) r[in->v1]++;
            else if (!strcmp(in->op,"dec")) r[in->v1]--;
            else if (!strcmp(in->op,"jnz")) {
                if (x1) {
                    long x2=in->t2? in->v2 : r[in->v2];
                    pc+=x2; pc--;
                }
            }
            else if (!strcmp(in->op,"tgl")) {
                int tgt=pc+x1;
                if (tgt>=0&&tgt<n){
                    I *t=&prog[tgt];
                    if (!strcmp(t->op,"inc")) strcpy(t->op,"dec");
                    else if (!strcmp(t->op,"dec")||!strcmp(t->op,"tgl")) strcpy(t->op,"inc");
                    else if (!strcmp(t->op,"jnz")) strcpy(t->op,"cpy");
                    else if (!strcmp(t->op,"cpy")) strcpy(t->op,"jnz");
                }
            }
        }
        printf("%ld\n",r[0]);
    }
    return 0;
}