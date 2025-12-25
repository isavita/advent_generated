
#import <Foundation/Foundation.h>

typedef struct { NSString *name; int a,b,c; } Inst;
typedef void (^Op)(int *r,int a,int b,int c);

int main(int argc,char **argv){
    @autoreleasepool{
        NSError *err=nil;
    NSString *txt=[NSString stringWithContentsOfFile:@"input.txt"
                                              encoding:NSUTF8StringEncoding
                                                 error:&err];
    NSArray *lines=[txt componentsSeparatedByString:@"\n"];
    int ip=[[[lines[0] componentsSeparatedByString:@" "] lastObject] intValue];
    Inst prog[lines.count-1];
    for(NSUInteger i=1;i<lines.count;i++){
        NSArray *w=[lines[i] componentsSeparatedByString:@" "];
        prog[i-1]=(Inst){w[0],[[w objectAtIndex:1] intValue],[[w objectAtIndex:2] intValue],[[w objectAtIndex:3] intValue]};
    }
    int r[6]={0};
    Op ops[16]={
        ^(int *r,int a,int b,int c){r[c]=r[a]+r[b];},        //addr
        ^(int *r,int a,int b,int c){r[c]=r[a]+b;},              //addi
        ^(int *r,int a,int b,int c){r[c]=r[a]*r[b];},          //mulr
        ^(int *r,int a,int b,int c){r[c]=r[a]*b;},              //muli
        ^(int *r,int a,int b,int c){r[c]=r[a]&r[b];},         //banr
        ^(int *r,int a,int b,int c){r[c]=r[a]&b;},             //bani
        ^(int *r,int a,int b,int c){r[c]=r[a]|r[b];},          //borr
        ^(int *r,int a,int b,int c){r[c]=r[a]|b;},              //bori
        ^(int *r,int a,int b,int c){r[c]=r[a];},                //setr
        ^(int *r,int a,int b,int c){r[c]=a;},                   //seti
        ^(int *r,int a,int b,int c){r[c]=a>r[b]?1:0;},        //gtir
        ^(int *r,int a,int b,int c){r[c]=r[a]>b?1:0;},         //gtri
        ^(int *r,int a,int b,int c){r[c]=r[a]>r[b]?1:0;},     //gtrr
        ^(int *r,int a,int b,int c){r[c]=a==r[b]?1:0;},       //eqir
        ^(int *r,int a,int b,int c){r[c]=r[a]==b?1:0;},        //eqri
        ^(int *r,int a,int b,int c){r[c]=r[a]==r[b]?1:0;}      //eqrr
    };
    NSDictionary *map=@{
        @"addr":ops[0],@"addi":ops[1],@"mulr":ops[2],@"muli":ops[3],
        @"banr":ops[4],@"bani":ops[5],@"borr":ops[6],@"bori":ops[7],
        @"setr":ops[8],@"seti":ops[9],
        @"gtir":ops[10],@"gtri":ops[11],@"gtrr":ops[12],
        @"eqir":ops[13],@"eqri":ops[14],@"eqrr":ops[15]
    };
    while(1){
        if(r[ip]==28) break;
        Inst I=prog[r[ip]];
        ((Op)map[I.name])(r,I.a,I.b,I.c);
        r[ip]++;
    }
    printf("%d\n",r[5]);
    }
    return 0;
}
