
#import <Foundation/Foundation.h>
#import <limits.h>

#define MAXR 64
#define MAXC 64
#define MAXLINE 1024

int R, C;
int matrix[MAXR][MAXC+1];
int colIsPivot[MAXC];
int freeVars[MAXC], nFree;
int target[MAXR], buttons[MAXC][MAXR], btnLen[MAXC];
int minWeight;

static inline int bits(int x){
    int c=0;
    while(x){ x&=(x-1); c++; }
    return c;
}

int gaussianEliminationMinWeight(){
    int pivotRow=0;
    memset(colIsPivot,0,sizeof(colIsPivot));
    for(int c=0;c<C && pivotRow<R;c++){
        int sel=-1;
        for(int r=pivotRow;r<R;r++)
            if(matrix[r][c]==1){ sel=r; break; }
        if(sel==-1) continue;
        for(int k=0;k<=C;k++){
            int t=matrix[pivotRow][k];
            matrix[pivotRow][k]=matrix[sel][k];
            matrix[sel][k]=t;
        }
        for(int r=0;r<R;r++){
            if(r!=pivotRow && matrix[r][c]==1){
                for(int k=c;k<=C;k++) matrix[r][k]^=matrix[pivotRow][k];
            }
        }
        colIsPivot[c]=1;
        pivotRow++;
    }
    for(int r=pivotRow;r<R;r++) if(matrix[r][C]==1) return -1;
    nFree=0;
    for(int c=0;c<C;c++) if(!colIsPivot[c]) freeVars[nFree++]=c;
    minWeight=INT_MAX;
    int limit=1<<nFree;
    for(int i=0;i<limit;i++){
        int x[MAXC]={0}, cw=bits(i);
        for(int j=0;j<nFree;j++) if((i>>j)&1) x[freeVars[j]]=1;
        int currPivotRow=0;
        for(int c=0;c<C;c++){
            if(colIsPivot[c]){
                int val=matrix[currPivotRow][C];
                for(int k=c+1;k<C;k++) if(matrix[currPivotRow][k]==1) val^=x[k];
                x[c]=val;
                if(val) cw++;
                currPivotRow++;
            }
        }
        if(cw<minWeight) minWeight=cw;
    }
    return minWeight;
}

int main(int argc, char *argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *err = nil;
        NSString *content = [NSString stringWithContentsOfFile:path
                                                   encoding:NSUTF8StringEncoding
                                                      error:&err];
        if (!content) {
            NSLog(@"Error reading file: %@", err);
            return 1;
        }
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        int totalPresses=0;
        for (NSString *line in lines) {
            NSRange open = [line rangeOfString:@"["];
            NSRange close = [line rangeOfString:@"]"];
            if (open.location == NSNotFound || close.location == NSNotFound) continue;
            NSString *t = [line substringWithRange:NSMakeRange(open.location+1, close.location-open.location-1)];
            R=(int)[t length];
            for(int i=0;i<R;i++) target[i]=([t characterAtIndex:i]=='#')?1:0;
            C=0;
            NSString *rest = [line substringFromIndex:close.location+1];
            NSRange p = [rest rangeOfString:@"("];
            while (p.location!=NSNotFound) {
                NSRange e = [rest rangeOfString:@")" options:0 range:NSMakeRange(p.location, rest.length-p.location)];
                if (e.location==NSNotFound) break;
                NSString *nums = [rest substringWithRange:NSMakeRange(p.location+1, e.location-p.location-1)];
                NSArray *parts = [nums componentsSeparatedByString:@","];
                btnLen[C]=0;
                for (NSString *part in parts) {
                    int idx = [part intValue];
                    buttons[C][btnLen[C]++]=idx;
                }
                C++;
                rest = [rest substringFromIndex:e.location+1];
                p = [rest rangeOfString:@"("];
            }
            for(int r=0;r<R;r++){
                for(int c=0;c<C;c++){
                    matrix[r][c]=0;
                    for(int k=0;k<btnLen[c];k++)
                        if(buttons[c][k]==r){ matrix[r][c]=1; break; }
                }
                matrix[r][C]=target[r];
            }
            int mw=gaussianEliminationMinWeight();
            if(mw!=-1) totalPresses+=mw;
        }
        printf("%d\n",totalPresses);
    }
    return 0;
}
