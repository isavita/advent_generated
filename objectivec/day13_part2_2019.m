
#import <Foundation/Foundation.h>
#define MEMORY_SIZE 10000
#define MAX_PROGRAM_SIZE 4096
typedef long long ll;
typedef struct{
    ll memory[MEMORY_SIZE];
    ll ip;
    ll relative_base;
    ll ball_x;
    ll paddle_x;
    ll score;
    int halted;
    ll temp_x;
    ll temp_y;
    int output_state;
} IntcodeComputer;
static inline ll get_addr(IntcodeComputer *c,int o,int m){
    ll p=c->ip+o;
    if(p<0||p>=MEMORY_SIZE){c->halted=1;return -1;}
    ll a;
    if(m==0)a=c->memory[p];
    else if(m==1)return p;
    else if(m==2)a=c->relative_base+c->memory[p];
    else{c->halted=1;return -1;}
    if(a<0||a>=MEMORY_SIZE){c->halted=1;return -1;}
    return a;
}
static inline ll get_param(IntcodeComputer *c,int o,int m){
    ll a=get_addr(c,o,m);
    if(c->halted||a==-1)return 0;
    return (m==1)?c->memory[a]:c->memory[a];
}
static inline ll get_write_addr(IntcodeComputer *c,int o,int m){
    if(m==1){c->halted=1;return -1;}
    return get_addr(c,o,m);
}
static void run_game(IntcodeComputer *c){
    while(!c->halted){
        ll instr=c->memory[c->ip];
        int op=instr%100;
        int m[3]={(instr/100)%10,(instr/1000)%10,(instr/10000)%10};
        ll a,p1,p2;
        switch(op){
            case 1:case 2:
                p1=get_param(c,1,m[0]);p2=get_param(c,2,m[1]);
                a=get_write_addr(c,3,m[2]);if(c->halted||a==-1)break;
                c->memory[a]=(op==1)?p1+p2:p1*p2;c->ip+=4;break;
            case 3:
                a=get_write_addr(c,1,m[0]);if(c->halted||a==-1)break;
                ll in=0;
                if(c->ball_x>c->paddle_x)in=1;
                else if(c->ball_x<c->paddle_x)in=-1;
                c->memory[a]=in;c->ip+=2;break;
            case 4:{
                ll out=get_param(c,1,m[0]);if(c->halted)break;
                if(c->output_state==0){c->temp_x=out;c->output_state=1;}
                else if(c->output_state==1){c->temp_y=out;c->output_state=2;}
                else{
                    if(c->temp_x==-1&&c->temp_y==0)c->score=out;
                    else if(out==3)c->paddle_x=c->temp_x;
                    else if(out==4)c->ball_x=c->temp_x;
                    c->output_state=0;
                }
                c->ip+=2;
                }break;
            case 5:case 6:
                p1=get_param(c,1,m[0]);p2=get_param(c,2,m[1]);if(c->halted)break;
                if((op==5&&p1!=0)||(op==6&&p1==0)){
                    if(p2<0||p2>=MEMORY_SIZE){c->halted=1;break;}
                    c->ip=p2;
                }else c->ip+=3;
                break;
            case 7:case 8:
                p1=get_param(c,1,m[0]);p2=get_param(c,2,m[1]);
                a=get_write_addr(c,3,m[2]);if(c->halted||a==-1)break;
                c->memory[a]=((op==7&&p1<p2)||(op==8&&p1==p2))?1:0;
                c->ip+=4;break;
            case 9:
                p1=get_param(c,1,m[0]);if(c->halted)break;
                c->relative_base+=p1;c->ip+=2;break;
            case 99:
                c->halted=1;break;
            default:
                c->halted=1;break;
        }
    }
}
int main(int argc, const char * argv[]){
    @autoreleasepool{
        FILE *f=fopen("input.txt","r");
        if(!f)return 1;
        ll prog[MAX_PROGRAM_SIZE];
        int sz=0;
        ll v;
        while(fscanf(f,"%lld,",&v)==1&&sz<MAX_PROGRAM_SIZE)prog[sz++]=v;
        fclose(f);
        if(sz==0)return 1;
        IntcodeComputer c={0};
        memcpy(c.memory,prog,sz*sizeof(ll));
        c.memory[0]=2;
        run_game(&c);
        printf("%lld\n",c.score);
    }
    return 0;
}
