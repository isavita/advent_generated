
#import <Foundation/Foundation.h>

#define MAXN 1024
#define MAXE 64

typedef struct { char name[32]; int nxt[MAXE],cnt; } Node;
static Node g[MAXN];
static int nidx,memo[MAXN];

static int find(NSString *s){
    const char *c=[s UTF8String];
    for(int i=0;i<nidx;i++)
        if(!strcmp(g[i].name,c)) return i;
    strcpy(g[nidx].name,c);
    return nidx++;
}

static int dfs(int u,int t){
    if(u==t) return 1;
    if(memo[u]>=0) return memo[u];
    int tot=0;
    for(int i=0;i<g[u].cnt;i++) tot+=dfs(g[u].nxt[i],t);
    return memo[u]=tot;
}

int main(int argc,char**argv){
    @autoreleasepool{
        NSString *txt=[NSString stringWithContentsOfFile:@"input.txt"
                              encoding:NSUTF8StringEncoding error:nil];
        for(NSString *l in [txt componentsSeparatedByString:@"\n"]){
            NSArray *a=[l componentsSeparatedByString:@":"];
            if(a.count<2) continue;
            int u=find(a[0]);
            for(NSString *w in [[a[1] stringByTrimmingCharactersInSet:
                                NSCharacterSet.whitespaceAndNewlineCharacterSet]
                               componentsSeparatedByString:@" "])
                g[u].nxt[g[u].cnt++]=find(w);
        }
        memset(memo,-1,sizeof(memo));
        printf("%d\n",dfs(find(@"you"),find(@"out")));
    }
    return 0;
}
