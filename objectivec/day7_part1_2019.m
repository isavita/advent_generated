
#import <Foundation/Foundation.h>

static NSArray<NSArray<NSNumber*>*>* permutations(NSArray<NSNumber*>* arr){
    if(arr.count==1) return @[arr];
    NSMutableArray* out=[NSMutableArray array];
    NSNumber* first=arr[0];
    for(NSArray* rest in permutations([arr subarrayWithRange:NSMakeRange(1,arr.count-1)])){
        for(NSUInteger i=0;i<=rest.count;i++){
            NSMutableArray* p=[rest mutableCopy];
            [p insertObject:first atIndex:i];
            [out addObject:p];
        }
    }
    return out;
}

static int runAmplifier(NSArray<NSString*>* program, int phase, int inputSignal){
    NSMutableArray* mem=[program mutableCopy];
    for(NSUInteger i=0;i<mem.count;i++) mem[i]=@([mem[i] intValue]);
    long long ptr=0,out=0,inputIdx=0;
    int inputs[2]={phase,inputSignal};
    while(1){
        int op=[mem[ptr] intValue];
        int opc=op%100, m1=(op/100)%10, m2=(op/1000)%10;
        if(opc==1){
            int v1=m1? [mem[ptr+1] intValue] : [mem[[mem[ptr+1] intValue]] intValue];
            int v2=m2? [mem[ptr+2] intValue] : [mem[[mem[ptr+2] intValue]] intValue];
            mem[[mem[ptr+3] intValue]]=@(v1+v2);
            ptr+=4;
        }else if(opc==2){
            int v1=m1? [mem[ptr+1] intValue] : [mem[[mem[ptr+1] intValue]] intValue];
            int v2=m2? [mem[ptr+2] intValue] : [mem[[mem[ptr+2] intValue]] intValue];
            mem[[mem[ptr+3] intValue]]=@(v1*v2);
            ptr+=4;
        }else if(opc==3){
            mem[[mem[ptr+1] intValue]]=@(inputs[inputIdx++]);
            ptr+=2;
        }else if(opc==4){
            out=m1? [mem[ptr+1] intValue] : [mem[[mem[ptr+1] intValue]] intValue];
            ptr+=2;
        }else if(opc==5){
            int v1=m1? [mem[ptr+1] intValue] : [mem[[mem[ptr+1] intValue]] intValue];
            int v2=m2? [mem[ptr+2] intValue] : [mem[[mem[ptr+2] intValue]] intValue];
            ptr=(v1!=0)? v2 : ptr+3;
        }else if(opc==6){
            int v1=m1? [mem[ptr+1] intValue] : [mem[[mem[ptr+1] intValue]] intValue];
            int v2=m2? [mem[ptr+2] intValue] : [mem[[mem[ptr+2] intValue]] intValue];
            ptr=(v1==0)? v2 : ptr+3;
        }else if(opc==7){
            int v1=m1? [mem[ptr+1] intValue] : [mem[[mem[ptr+1] intValue]] intValue];
            int v2=m2? [mem[ptr+2] intValue] : [mem[[mem[ptr+2] intValue]] intValue];
            mem[[mem[ptr+3] intValue]]=@(v1<v2?1:0);
            ptr+=4;
        }else if(opc==8){
            int v1=m1? [mem[ptr+1] intValue] : [mem[[mem[ptr+1] intValue]] intValue];
            int v2=m2? [mem[ptr+2] intValue] : [mem[[mem[ptr+2] intValue]] intValue];
            mem[[mem[ptr+3] intValue]]=@(v1==v2?1:0);
            ptr+=4;
        }else if(opc==99){
            break;
        }
    }
    return (int)out;
}

int main(int argc,const char* argv[]){
    @autoreleasepool {
        NSString* txt=[NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString*>* parts=[txt componentsSeparatedByString:@","];
        NSArray<NSNumber*>* phases=@[@0,@1,@2,@3,@4];
        int max=0;
        for(NSArray<NSNumber*>* perm in permutations(phases)){
            int sig=0;
            for(NSNumber* p in perm) sig=runAmplifier(parts,[p intValue],sig);
            if(sig>max) max=sig;
        }
        printf("%d\n",max);
    }
    return 0;
}
