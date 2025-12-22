
#import <Foundation/Foundation.h>

#define MAX 1000
typedef struct { char op[4], a[4], b[4]; } Ins;
static Ins prog[MAX]; static int pc=0, r[4];

static int v(NSString *s){
    unichar c=[s characterAtIndex:0];
    if(isdigit(c) || (c=='-'&&[s length]>1)) return [s intValue];
    return r[c-'a'];
}

static BOOL ok(){
    int last=-1, cnt=0, ip=0;
    while(ip<pc){
        Ins *I=&prog[ip];
        if(!strcmp(I->op,"cpy")) r[I->b[0]-'a']=v(@(I->a));
        else if(!strcmp(I->op,"inc")) r[I->a[0]-'a']++;
        else if(!strcmp(I->op,"dec")) r[I->a[0]-'a']--;
        else if(!strcmp(I->op,"jnz")){
            if(v(@(I->a))){ ip+=v(@(I->b)); continue; }
        }else if(!strcmp(I->op,"out")){
            int x=v(@(I->a));
            if(x!=0&&x!=1) return NO;
            if(cnt&&x==last) return NO;
            last=x; if(++cnt>50) return YES;
        }
        ip++;
    }
    return NO;
}

int main(int argc,char**argv){
    @autoreleasepool{
        NSError *e=nil;
        NSString *f=[NSString stringWithContentsOfFile:@"input.txt"
                   encoding:NSUTF8StringEncoding error:&e];
        if(!f){ NSLog(@"%@",e); return 1; }
        for(NSString *l in [f componentsSeparatedByString:@"\n"]){
            NSArray *p=[l componentsSeparatedByString:@" "];
            if([p count]<2) continue;
            strcpy(prog[pc].op, [[p objectAtIndex:0] UTF8String]);
            strcpy(prog[pc].a,  [[p objectAtIndex:1] UTF8String]);
            if([p count]>2) strcpy(prog[pc].b, [[p objectAtIndex:2] UTF8String]);
            pc++;
        }
        for(int a=0;;a++){
            r[0]=a; r[1]=r[2]=r[3]=0;
            if(ok()){ printf("%d\n",a); return 0; }
        }
    }
}
