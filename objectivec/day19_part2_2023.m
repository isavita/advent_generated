
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <stdint.h>
#import <stdbool.h>

#define MAX_NAME_LEN 10
#define MAX_RULES 10
#define MAX_WORKFLOWS 600
#define HASH_TABLE_SIZE (1 << 20)

typedef int64_t i64;

typedef struct { int min; int max; } Range;
typedef struct { Range ranges[4]; } Constraints;
typedef struct {
    char variable;
    char op;
    int value;
    int dest_id;
} Rule;
typedef struct {
    char name[MAX_NAME_LEN];
    Rule rules[MAX_RULES];
    int num_rules;
} Workflow;
typedef struct { int workflow_id; Constraints constraints; } MemoKey;
typedef struct { MemoKey key; i64 value; bool occupied; } MemoEntry;

static Workflow workflows[MAX_WORKFLOWS];
static int workflow_count = 0;
static NSMutableDictionary<NSString*, NSNumber*> *nameMap;
static MemoEntry memo_table[HASH_TABLE_SIZE];

static int getWorkflowId(const char *name) {
    if (strcmp(name, "A") == 0) return -1;
    if (strcmp(name, "R") == 0) return -2;
    NSString *nsName = [NSString stringWithUTF8String:name];
    NSNumber *num = nameMap[nsName];
    if (num) return [num intValue];
    int id = workflow_count;
    nameMap[nsName] = @(id);
    workflow_count++;
    return id;
}
static int charToIndex(char v) {
    return (v=='x')?0:(v=='m')?1:(v=='a')?2:(v=='s')?3:-1;
}
static uint32_t hashConstraints(Constraints c) {
    uint32_t h = 0x811c9dc5;
    for (int i=0;i<4;i++) {
        h ^= (uint32_t)c.ranges[i].min; h *= 0x01000193;
        h ^= (uint32_t)c.ranges[i].max; h *= 0x01000193;
    }
    return h;
}
static uint32_t hashKey(MemoKey k) {
    uint32_t h = hashConstraints(k.constraints);
    h ^= (uint32_t)k.workflow_id; h *= 0x01000193;
    return h;
}
static bool compareKeys(MemoKey a, MemoKey b) {
    if (a.workflow_id!=b.workflow_id) return false;
    for (int i=0;i<4;i++) if (a.constraints.ranges[i].min!=b.constraints.ranges[i].min||a.constraints.ranges[i].max!=b.constraints.ranges[i].max) return false;
    return true;
}
static bool memoLookup(MemoKey k,i64 *v) {
    uint32_t idx = hashKey(k)&(HASH_TABLE_SIZE-1);
    for (int i=0;i<HASH_TABLE_SIZE;i++) {
        uint32_t cur=(idx+i)&(HASH_TABLE_SIZE-1);
        if (!memo_table[cur].occupied) return false;
        if (compareKeys(memo_table[cur].key,k)) { *v=memo_table[cur].value; return true; }
    }
    return false;
}
static void memoInsert(MemoKey k,i64 v) {
    uint32_t idx = hashKey(k)&(HASH_TABLE_SIZE-1);
    for (int i=0;i<HASH_TABLE_SIZE;i++) {
        uint32_t cur=(idx+i)&(HASH_TABLE_SIZE-1);
        if (!memo_table[cur].occupied) {
            memo_table[cur].key=k; memo_table[cur].value=v; memo_table[cur].occupied=true;
            return;
        }
    }
}
static i64 countComb(Constraints c) {
    i64 t=1;
    for (int i=0;i<4;i++) {
        i64 sz=(i64)c.ranges[i].max-c.ranges[i].min+1;
        if (sz<=0) return 0;
        t*=sz;
    }
    return t;
}
static i64 process(int wid, Constraints cons) {
    if (wid==-2) return 0;
    if (wid==-1) return countComb(cons);
    for (int i=0;i<4;i++) if (cons.ranges[i].min>cons.ranges[i].max) return 0;
    MemoKey key={wid,cons}; i64 cached;
    if (memoLookup(key,&cached)) return cached;
    i64 res=0;
    Workflow *wf=&workflows[wid];
    Constraints cur=cons;
    for (int i=0;i<wf->num_rules;i++) {
        Rule *r=&wf->rules[i];
        if (r->variable==0) { res+=process(r->dest_id,cur); break; }
        int vi=charToIndex(r->variable);
        Range vr=cur.ranges[vi];
        Range tr={0,-1}, fr={0,-1};
        if (r->op=='<') { tr.min=vr.min; tr.max=r->value-1; fr.min=r->value; fr.max=vr.max; }
        else { tr.min=r->value+1; tr.max=vr.max; fr.min=vr.min; fr.max=r->value; }
        if (tr.min<vr.min) tr.min=vr.min; if (tr.max>vr.max) tr.max=vr.max;
        if (fr.min<vr.min) fr.min=vr.min; if (fr.max>vr.max) fr.max=vr.max;
        if (tr.min<=tr.max) {
            Constraints nxt=cur; nxt.ranges[vi]=tr;
            res+=process(r->dest_id,nxt);
        }
        if (fr.min<=fr.max) { cur.ranges[vi]=fr; }
        else break;
    }
    memoInsert(key,res);
    return res;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        nameMap=[[NSMutableDictionary alloc] init];
        NSString *txt=[NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString*> *lines=[txt componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSUInteger idx=0;
        for (; idx<lines.count; idx++) {
            NSString *line=lines[idx];
            if (line.length==0) break;
            NSRange brace=[line rangeOfString:@"{"];
            if (brace.location==NSNotFound) continue;
            NSString *name=[line substringToIndex:brace.location];
            NSString *rulesStr=[[line substringFromIndex:brace.location+1] stringByReplacingOccurrencesOfString:@"}" withString:@""];
            int wid=getWorkflowId(name.UTF8String);
            strcpy(workflows[wid].name,name.UTF8String);
            workflows[wid].num_rules=0;
            NSArray<NSString*> *tokens=[rulesStr componentsSeparatedByString:@","];
            for (NSString *tok in tokens) {
                Rule *r=&workflows[wid].rules[workflows[wid].num_rules++];
                NSRange colon=[tok rangeOfString:@":"];
                if (colon.location!=NSNotFound) {
                    NSString *cond=[tok substringToIndex:colon.location];
                    NSString *dest=[tok substringFromIndex:colon.location+1];
                    r->dest_id=getWorkflowId(dest.UTF8String);
                    r->variable=[cond characterAtIndex:0];
                    r->op=[cond characterAtIndex:1];
                    r->value=[[cond substringFromIndex:2] intValue];
                } else {
                    r->variable=0; r->op=0; r->value=0;
                    r->dest_id=getWorkflowId(tok.UTF8String);
                }
            }
        }
        memset(memo_table,0,sizeof(memo_table));
        Constraints init; for (int i=0;i<4;i++){init.ranges[i].min=1;init.ranges[i].max=4000;}
        int start=getWorkflowId("in");
        i64 ans=process(start,init);
        printf("%lld\n",ans);
    }
    return 0;
}
