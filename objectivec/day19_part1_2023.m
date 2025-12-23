
#import <Foundation/Foundation.h>

#define MAX_LINE_LENGTH 256
#define MAX_WORKFLOWS 600
#define MAX_RULES 10
#define MAX_PARTS 300
#define MAX_NAME_LEN 4

typedef struct {
    int x,m,a,s;
} Part;

typedef struct {
    char category;
    char op;
    int value;
    short destIdx;
} Rule;

typedef struct {
    char name[MAX_NAME_LEN];
    Rule rules[MAX_RULES];
    int numRules;
} Workflow;

static Workflow workflows[MAX_WORKFLOWS];
static int workflowCount = 0;
static Part parts[MAX_PARTS];
static int partCount = 0;
static NSMutableDictionary<NSString*, NSNumber*> *nameToIdx;

static short getWorkflowIndex(const char *name) {
    if (name[0]=='A' && name[1]==0) return -1;
    if (name[0]=='R' && name[1]==0) return -2;
    NSString *key = [NSString stringWithUTF8String:name];
    NSNumber *num = nameToIdx[key];
    if (num) return (short)[num shortValue];
    if (workflowCount>=MAX_WORKFLOWS) return -3;
    strncpy(workflows[workflowCount].name, name, MAX_NAME_LEN-1);
    workflows[workflowCount].name[MAX_NAME_LEN-1]=0;
    workflows[workflowCount].numRules=0;
    nameToIdx[key]=@(workflowCount);
    return (short)workflowCount++;
}

static void parseRule(Workflow *wf, const char *str) {
    Rule *r=&wf->rules[wf->numRules++];
    char dest[MAX_NAME_LEN];
    const char *colon=strchr(str,':');
    if (colon) {
        r->category=str[0];
        r->op=str[1];
        r->value=atoi(str+2);
        strncpy(dest, colon+1, MAX_NAME_LEN-1);
    } else {
        r->category=0;
        r->op=0;
        r->value=0;
        strncpy(dest, str, MAX_NAME_LEN-1);
    }
    dest[MAX_NAME_LEN-1]=0;
    r->destIdx=getWorkflowIndex(dest);
}

static void parseInput(const char *filename) {
    NSString *content=[NSString stringWithContentsOfFile:@(filename) encoding:NSUTF8StringEncoding error:nil];
    NSArray<NSString*> *lines=[content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
    BOOL parsingWorkflows=YES;
    for (NSString *raw in lines) {
        NSString *line=[raw stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
        if (line.length==0) {parsingWorkflows=NO; continue;}
        if (parsingWorkflows) {
            NSRange brace=[line rangeOfString:@"{"];
            if (brace.location==NSNotFound) continue;
            NSString *name=[line substringToIndex:brace.location];
            const char *cname=name.UTF8String;
            short idx=getWorkflowIndex(cname);
            Workflow *wf=&workflows[idx];
            wf->numRules=0;
            NSString *rulesStr=[line substringFromIndex:brace.location+1];
            NSCharacterSet *sep=[NSCharacterSet characterSetWithCharactersInString:@",}"];
            NSArray<NSString*> *tokens=[rulesStr componentsSeparatedByCharactersInSet:sep];
            for (NSString *t in tokens) {
                if (t.length) parseRule(wf, t.UTF8String);
            }
        } else {
            if (partCount>=MAX_PARTS) continue;
            int x,m,a,s;
            sscanf(line.UTF8String, "{x=%d,m=%d,a=%d,s=%d}", &x,&m,&a,&s);
            parts[partCount++] = (Part){x,m,a,s};
        }
    }
}

static BOOL evaluate(const Part *p, short startIdx) {
    short cur=startIdx;
    while (cur>=0) {
        const Workflow *wf=&workflows[cur];
        for (int i=0;i<wf->numRules;i++) {
            const Rule *r=&wf->rules[i];
            BOOL cond=NO;
            if (r->category==0) cond=YES;
            else {
                int v;
                switch(r->category){case 'x':v=p->x;break;case 'm':v=p->m;break;case 'a':v=p->a;break;case 's':v=p->s;break;default:v=0;}
                if (r->op=='<') cond=v<r->value;
                else if (r->op=='>') cond=v>r->value;
            }
            if (cond) {cur=r->destIdx; break;}
        }
        if (cur==-1) return YES;
        if (cur==-2) return NO;
    }
    return NO;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        nameToIdx=[NSMutableDictionary dictionary];
        parseInput("input.txt");
        long long total=0;
        short inIdx=getWorkflowIndex("in");
        for (int i=0;i<partCount;i++) if (evaluate(&parts[i], inIdx)) total+=parts[i].x+parts[i].m+parts[i].a+parts[i].s;
        printf("%lld\n", total);
    }
    return 0;
}
