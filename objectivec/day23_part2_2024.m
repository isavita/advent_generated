#import <Foundation/Foundation.h>

#define MAXN 2048
static char names[MAXN][16];
static BOOL adj[MAXN][MAXN];
static int ncnt=0, bestv[MAXN], bestn=0;

static int idFor(NSString *s){
    const char *c=[s UTF8String];
    for(int i=0;i<ncnt;i++)
        if(strcmp(names[i],c)==0) return i;
    if(ncnt==MAXN){ NSLog(@"Too many nodes"); exit(1); }
    strcpy(names[ncnt],c);
    return ncnt++;
}
static void bron(int *R,int rn,int *P,int pn){
    if(pn==0){
        if(rn>bestn){ bestn=rn; memcpy(bestv,R,rn*sizeof(int)); }
        return;
    }
    if(rn+pn<=bestn) return;
    int v=P[0], newP[pn], newn=0;
    for(int i=0;i<pn;i++){
        int u=P[i];
        if(!adj[v][u]) continue;
        newP[newn++]=u;
    }
    R[rn]=v; bron(R,rn+1,newP,newn);
    bron(R,rn,P+1,pn-1);
}
int main(int argc,char **argv){
    @autoreleasepool{
        NSString *txt=[NSString stringWithContentsOfFile:@"input.txt"
                                                 encoding:NSUTF8StringEncoding error:nil];
        if(!txt){ NSLog(@"Can't read input.txt"); return 1; }
        for(NSString *line in [txt componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]]){
            if(line.length==0) continue;
            NSArray *a=[line componentsSeparatedByString:@"-"];
            if(a.count!=2) continue;
            int x=idFor(a[0]), y=idFor(a[1]);
            adj[x][y]=adj[y][x]=YES;
        }
        int P[MAXN], R[1];
        for(int i=0;i<ncnt;i++) P[i]=i;
        bron(R,0,P,ncnt);
        NSMutableArray *out=[NSMutableArray arrayWithCapacity:bestn];
        for(int i=0;i<bestn;i++) [out addObject:[NSString stringWithUTF8String:names[bestv[i]]]];
        NSArray *sorted=[out sortedArrayUsingSelector:@selector(compare:)];
        printf("%s\n",[[sorted componentsJoinedByString:@","] UTF8String]);
    } return 0;
}