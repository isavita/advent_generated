
#import <Foundation/Foundation.h>

#define MAX 1000
#define LEN 50

typedef struct { char op[4], a1[4], a2[4]; } Ins;
Ins prog[MAX]; int pcnt=0, reg[26];

static inline int val(NSString *s){
    unichar c=[s characterAtIndex:0];
    return (isdigit(c)||(c=='-'&&[s length]>1&&isdigit([s characterAtIndex:1])))?[s intValue]:reg[c-'a'];
}

static inline void tgl(Ins *I){
    if(!strcmp(I->op,"inc")) strcpy(I->op,"dec");
    else if(!strcmp(I->op,"dec")||!strcmp(I->op,"tgl")) strcpy(I->op,"inc");
    else if(!strcmp(I->op,"jnz")) strcpy(I->op,"cpy");
    else if(!strcmp(I->op,"cpy")) strcpy(I->op,"jnz");
}

int main(int argc,char**argv){
    @autoreleasepool{
        NSError *e=nil;
        NSString *txt=[NSString stringWithContentsOfFile:@"input.txt"
                                                 encoding:NSUTF8StringEncoding error:&e];
        if(!txt){NSLog(@"%@",e);return 1;}
        NSArray *lines=[txt componentsSeparatedByCharactersInSet:
                       [NSCharacterSet newlineCharacterSet]];
        for(NSString *l in lines){
            NSArray *tok=[l componentsSeparatedByString:@" "];
            if(tok.count<2) continue;
            strcpy(prog[pcnt].op, [tok[0] UTF8String]);
            strcpy(prog[pcnt].a1, [tok[1] UTF8String]);
            strcpy(prog[pcnt].a2, tok.count>2?[tok[2] UTF8String]:"");
            pcnt++;
        }
        reg['a'-'a']=7;
        int ip=0;
        while(ip<pcnt){
            Ins *I=&prog[ip];
            if(!strcmp(I->op,"cpy")){
                if(isalpha(I->a2[0])) reg[I->a2[0]-'a']=val(@(I->a1));
            }else if(!strcmp(I->op,"inc")){
                if(isalpha(I->a1[0])) reg[I->a1[0]-'a']++;
            }else if(!strcmp(I->op,"dec")){
                if(isalpha(I->a1[0])) reg[I->a1[0]-'a']--;
            }else if(!strcmp(I->op,"jnz")){
                if(val(@(I->a1))!=0) ip+=val(@(I->a2))-1;
            }else if(!strcmp(I->op,"tgl")){
                int tgt=ip+val(@(I->a1));
                if(tgt>=0&&tgt<pcnt) tgl(&prog[tgt]);
            }
            ip++;
        }
        printf("%d\n",reg['a'-'a']);
    }
    return 0;
}
