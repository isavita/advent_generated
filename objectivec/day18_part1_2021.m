
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct SnailNumber {
    int value;
    struct SnailNumber *left;
    struct SnailNumber *right;
} SnailNumber;

static SnailNumber* create_regular(int v){
    SnailNumber* s=malloc(sizeof(SnailNumber));
    s->value=v; s->left=NULL; s->right=NULL;
    return s;
}
static SnailNumber* create_pair(SnailNumber* l,SnailNumber* r){
    SnailNumber* s=malloc(sizeof(SnailNumber));
    s->value=-1; s->left=l; s->right=r;
    return s;
}
static int is_regular(SnailNumber* s){return s->left==NULL && s->right==NULL;}
static SnailNumber* parse(char** p){
    if(**p=='['){
        (*p)++;
        SnailNumber* l=parse(p);
        (*p)++;
        SnailNumber* r=parse(p);
        (*p)++;
        return create_pair(l,r);
    }else{
        int v=0;
        while(isdigit(**p)){v=v*10+(**p-'0');(*p)++;}
        return create_regular(v);
    }
}
static void add_left(SnailNumber* s,int v){
    if(is_regular(s))s->value+=v;
    else add_left(s->left,v);
}
static void add_right(SnailNumber* s,int v){
    if(is_regular(s))s->value+=v;
    else add_right(s->right,v);
}
static int explode(SnailNumber* s,int d,int* l,int* r){
    if(is_regular(s))return 0;
    if(d==4){
        *l=s->left->value; *r=s->right->value;
        free(s->left); free(s->right);
        s->left=s->right=NULL; s->value=0;
        return 1;
    }
    int ll=0,rr=0;
    if(explode(s->left,d+1,&ll,&rr)){
        if(rr)add_left(s->right,rr);
        *l=ll; *r=0;
        return 1;
    }
    if(explode(s->right,d+1,&ll,&rr)){
        if(ll)add_right(s->left,ll);
        *l=0; *r=rr;
        return 1;
    }
    return 0;
}
static int split(SnailNumber* s){
    if(is_regular(s)){
        if(s->value>=10){
            int l=s->value/2;
            int r=(s->value+1)/2;
            s->left=create_regular(l);
            s->right=create_regular(r);
            s->value=-1;
            return 1;
        }
        return 0;
    }
    return split(s->left)||split(s->right);
}
static void reduce(SnailNumber* s){
    while(1){
        int l=0,r=0;
        if(explode(s,0,&l,&r))continue;
        if(split(s))continue;
        break;
    }
}
static SnailNumber* add(SnailNumber* a,SnailNumber* b){
    SnailNumber* n=create_pair(a,b);
    reduce(n);
    return n;
}
static long long magnitude(SnailNumber* s){
    if(is_regular(s))return s->value;
    return 3*magnitude(s->left)+2*magnitude(s->right);
}
static void free_sn(SnailNumber* s){
    if(!s)return;
    if(!is_regular(s)){free_sn(s->left);free_sn(s->right);}
    free(s);
}

int main(int argc, const char * argv[]) {
    FILE* f=fopen("input.txt","r");
    if(!f)return 1;
    SnailNumber* arr[256];
    int cnt=0;
    char line[512];
    while(fgets(line,sizeof(line),f)){
        line[strcspn(line,"\r\n")]=0;
        if(*line){
            char* p=line;
            arr[cnt++]=parse(&p);
        }
    }
    fclose(f);
    if(cnt==0)return 0;
    SnailNumber* res=arr[0];
    for(int i=1;i<cnt;i++)res=add(res,arr[i]);
    printf("%lld\n",magnitude(res));
    free_sn(res);
    return 0;
}
