
#import <Foundation/Foundation.h>

int A,B,C,ip=0,programSize=0,outputLen=0;
int program[1024];
char output[1024];

static inline int combo(int op){
    switch(op){
        case 0 ... 3: return op;
        case 4: return A;
        case 5: return B;
        case 6: return C;
        default: return 0;
    }
}

int main(int argc,char**argv){
    @autoreleasepool{
        NSString*txt=[NSString stringWithContentsOfFile:@"input.txt"
                                               encoding:NSUTF8StringEncoding error:nil];
        for(NSString*line in [txt componentsSeparatedByString:@"\n"]){
            if([line hasPrefix:@"Register A:"]) A=[[line substringFromIndex:11] intValue];
            else if([line hasPrefix:@"Register B:"]) B=[[line substringFromIndex:11] intValue];
            else if([line hasPrefix:@"Register C:"]) C=[[line substringFromIndex:11] intValue];
            else if([line hasPrefix:@"Program:"]){
                for(NSString*s in [[line substringFromIndex:8] componentsSeparatedByString:@","])
                    program[programSize++]=[s intValue];
            }
        }
        while(ip<programSize){
            int op=program[ip],v=program[++ip];
            switch(op){
                case 0: A>>=combo(v);break;
                case 1: B^=v;break;
                case 2: B=combo(v)&7;break;
                case 3: if(A){ip=v;continue;}break;
                case 4: B^=C;break;
                case 5: outputLen+=sprintf(output+outputLen,"%d,",combo(v)&7);break;
                case 6: B=A/(1<<combo(v));break;
                case 7: C=A/(1<<combo(v));break;
            }
            ++ip;
        }
        if(outputLen) output[outputLen-1]=0;
        printf("%s\n",output);
    }
    return 0;
}
