
#import <Foundation/Foundation.h>
typedef unsigned long long u64;
static inline u64 mod_add(u64 a,u64 b,u64 m){
    a%=m;b%=m;
    u64 r=a+b;
    if(r<a||r>=m)r-=m;
    return r;
}
static inline u64 mod_sub(u64 a,u64 b,u64 m){
    a%=m;b%=m;
    return (a+m-b)%m;
}
static inline u64 mod_mul(u64 a,u64 b,u64 m){
#ifdef __SIZEOF_INT128__
    __uint128_t r=(__uint128_t)a*b;
    return (u64)(r%m);
#else
    u64 r=0;
    a%=m;
    while(b){
        if(b&1)r=mod_add(r,a,m);
        a=mod_add(a,a,m);
        b>>=1;
    }
    return r;
#endif
}
static u64 mod_pow(u64 base,u64 exp,u64 mod){
    u64 r=1;
    base%=mod;
    while(exp){
        if(exp&1)r=mod_mul(r,base,mod);
        base=mod_mul(base,base,mod);
        exp>>=1;
    }
    return r;
}
static inline u64 mod_inverse(u64 n,u64 mod){
    return mod_pow(n,mod-2,mod);
}
int main(int argc,const char *argv[]){
    FILE *f=fopen("input.txt","r");
    if(!f)return 1;
    const u64 SIZE=119315717514047ULL;
    const u64 ITER=101741582076661ULL;
    const u64 TARGET=2020ULL;
    u64 offset=0,inc=1;
    char line[128];
    while(fgets(line,sizeof(line),f)){
        line[strcspn(line,"\n")]=0;
        if(strcmp(line,"deal into new stack")==0){
            inc=mod_mul(inc,SIZE-1,SIZE);
            offset=mod_add(offset,inc,SIZE);
        }else if(strncmp(line,"cut",3)==0){
            long long nll;
            sscanf(line,"cut %lld",&nll);
            u64 n=nll>=0?(u64)nll:(SIZE-(u64)(-nll));
            n%=SIZE;
            offset=mod_add(offset,mod_mul(n,inc,SIZE),SIZE);
        }else if(strncmp(line,"deal with increment",19)==0){
            u64 n;
            sscanf(line,"deal with increment %llu",&n);
            inc=mod_mul(inc,mod_inverse(n,SIZE),SIZE);
        }
    }
    fclose(f);
    u64 a=mod_pow(inc,ITER,SIZE);
    u64 b=mod_mul(offset,mod_mul(mod_sub(a,1,SIZE),mod_inverse(mod_sub(inc,1,SIZE),SIZE),SIZE),SIZE);
    u64 ans=mod_add(mod_mul(TARGET,a,SIZE),b,SIZE);
    printf("%llu\n",ans);
    return 0;
}
