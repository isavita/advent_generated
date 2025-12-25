
#import <Foundation/Foundation.h>

static const char OPEN='.';
static const char TREES='|';
static const char LUMBERYARD='#';

static char **readInput(NSString *path, NSInteger *rows, NSInteger *cols){
    NSString *content=[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
    NSArray<NSString *> *lines=[content componentsSeparatedByString:@"\n"];
    if(lines.lastObject.length==0) lines=[lines subarrayWithRange:NSMakeRange(0, lines.count-1)];
    *rows=lines.count;
    *cols=lines[0].length;
    char **grid=malloc(sizeof(char*)*(*rows));
    for(NSInteger i=0;i<*rows;i++){
        grid[i]=malloc(sizeof(char)*(*cols));
        const char *cstr=[lines[i] UTF8String];
        memcpy(grid[i], cstr, *cols);
    }
    return grid;
}

static NSInteger countAdjacent(char **g, NSInteger r, NSInteger c, NSInteger rows, NSInteger cols, char type){
    NSInteger cnt=0;
    for(NSInteger dr=-1;dr<=1;dr++)for(NSInteger dc=-1;dc<=1;dc++){
        if(dr==0&&dc==0)continue;
        NSInteger nr=r+dr,nc=c+dc;
        if(nr>=0&&nr<rows&&nc>=0&&nc<cols&&g[nr][nc]==type)cnt++;
    }
    return cnt;
}

static char nextState(char **g, NSInteger r, NSInteger c, NSInteger rows, NSInteger cols){
    char cur=g[r][c];
    if(cur==OPEN) return countAdjacent(g,r,c,rows,cols,TREES)>=3?TREES:OPEN;
    if(cur==TREES) return countAdjacent(g,r,c,rows,cols,LUMBERYARD)>=3?LUMBERYARD:TREES;
    return (countAdjacent(g,r,c,rows,cols,LUMBERYARD)>=1 && countAdjacent(g,r,c,rows,cols,TREES)>=1)?LUMBERYARD:OPEN;
}

static char **transform(char **g, NSInteger rows, NSInteger cols){
    char **ng=malloc(sizeof(char*)*rows);
    for(NSInteger i=0;i<rows;i++){
        ng[i]=malloc(sizeof(char)*cols);
        for(NSInteger j=0;j<cols;j++) ng[i][j]=nextState(g,i,j,rows,cols);
    }
    return ng;
}

static NSString *gridKey(char **g, NSInteger rows, NSInteger cols){
    NSMutableData *data=[NSMutableData dataWithLength:rows*cols];
    for(NSInteger i=0;i<rows;i++) memcpy(data.mutableBytes+i*cols, g[i], cols);
    return [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
}

static void countResources(char **g, NSInteger rows, NSInteger cols, NSInteger *wooded, NSInteger *lumber){
    *wooded=*lumber=0;
    for(NSInteger i=0;i<rows;i++)for(NSInteger j=0;j<cols;j++){
        if(g[i][j]==TREES) (*wooded)++;
        else if(g[i][j]==LUMBERYARD) (*lumber)++;
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool{
        NSInteger rows,cols;
        char **grid=readInput(@"input.txt",&rows,&cols);
        NSMutableDictionary<NSString*,NSNumber*> *seen=[NSMutableDictionary dictionary];
        NSInteger minute=0,cycleStart=0,cycleLen=0;
        while(YES){
            NSString *key=gridKey(grid,rows,cols);
            NSNumber *prev=[seen objectForKey:key];
            if(prev){
                cycleStart=prev.integerValue;
                cycleLen=minute-cycleStart;
                break;
            }
            seen[key]=@(minute);
            char **next=transform(grid,rows,cols);
            for(NSInteger i=0;i<rows;i++) free(grid[i]);
            free(grid);
            grid=next;
            minute++;
        }
        NSInteger remaining=(1000000000LL-cycleStart)%cycleLen;
        for(NSInteger i=0;i<remaining;i++){
            char **next=transform(grid,rows,cols);
            for(NSInteger r=0;r<rows;r++) free(grid[r]);
            free(grid);
            grid=next;
        }
        NSInteger wooded,lumber;
        countResources(grid,rows,cols,&wooded,&lumber);
        printf("%lld\n",(long long)(wooded*lumber));
        for(NSInteger i=0;i<rows;i++) free(grid[i]);
        free(grid);
    }
    return 0;
}
