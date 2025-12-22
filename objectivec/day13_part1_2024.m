
#import <Foundation/Foundation.h>

int solveMachine(int ax,int ay,int bx,int by,int px,int py){
    int minCost=-1;
    for(int a=0;a<=100;++a){
        for(int b=0;b<=100;++b){
            int x=ax*a+bx*b;
            int y=ay*a+by*b;
            if(x==px && y==py){
                int cost=a*3+b;
                if(minCost==-1 || cost<minCost) minCost=cost;
            }
        }
    }
    return minCost;
}

int main(int argc, const char * argv[]){
    @autoreleasepool{
        NSString *path=@"input.txt";
        NSString *content=[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if(!content) return 1;
        NSArray *rawLines=[content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        int solved=0;
        long long total=0;
        int ax=0,ay=0,bx=0,by=0,px=0,py=0;
        BOOL haveA=NO,haveB=NO,haveP=NO,hasLines=NO;
        for(NSString *raw in rawLines){
            NSString *line=[[raw stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]] copy];
            if(line.length==0){
                if(hasLines && haveA && haveB && haveP){
                    int cost=solveMachine(ax,ay,bx,by,px,py);
                    if(cost!=-1){ solved++; total+=cost; }
                }
                hasLines=NO; haveA=haveB=haveP=NO;
                continue;
            }
            hasLines=YES;
            const char *c=[line UTF8String];
            if(sscanf(c," Button A: X+%d, Y+%d",&ax,&ay)==2 ||
               sscanf(c," A: X+%d, Y+%d",&ax,&ay)==2){
                haveA=YES;
            }else if(sscanf(c," Button B: X+%d, Y+%d",&bx,&by)==2 ||
                     sscanf(c," B: X+%d, Y+%d",&bx,&by)==2){
                haveB=YES;
            }else if(sscanf(c," Prize: X=%d, Y=%d",&px,&py)==2 ||
                     sscanf(c," P: X=%d, Y=%d",&px,&py)==2){
                haveP=YES;
            }
        }
        if(hasLines && haveA && haveB && haveP){
            int cost=solveMachine(ax,ay,bx,by,px,py);
            if(cost!=-1){ solved++; total+=cost; }
        }
        printf("%d %lld\n",solved,total);
    }
    return 0;
}
