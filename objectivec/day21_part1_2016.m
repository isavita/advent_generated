
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <string.h>

static void swapPos(char *p,int x,int y){char t=p[x];p[x]=p[y];p[y]=t;}
static void swapLet(char *p,char x,char y){for(int i=0;p[i];i++){if(p[i]==x)p[i]=y;else if(p[i]==y)p[i]=x;}}
static void rotL(char *p,int s,int l){char t[256];memcpy(t,p+s,l-s);memcpy(t+l-s,p,s);t[l]=0;memcpy(p,t,l+1);}
static void rotR(char *p,int s,int l){char t[256];memcpy(t,p+l-s,s);memcpy(t+s,p,l-s);t[l]=0;memcpy(p,t,l+1);}
static void rotBased(char *p,char x){int i=strchr(p,x)-p;int s=1+i+(i>=4);rotR(p,s%strlen(p),strlen(p));}
static void revPos(char *p,int x,int y){while(x<y){swapPos(p,x++,y--);}}
static void movePos(char *p,int x,int y){char c=p[x];memmove(p+x,p+x+1,strlen(p)-x);memmove(p+y+1,p+y,strlen(p)-y);p[y]=c;}

int main(int argc,const char *argv[]){
    @autoreleasepool{
        FILE *f=fopen("input.txt","r");
        if(!f){printf("Error reading input file\n");return 1;}
        char pwd[]="abcdefgh";
        char op[256];
        while(fgets(op,sizeof op,f)){
            op[strcspn(op,"\n")]=0;
            int a,b;char s1[32],s2[32];
            if(sscanf(op,"swap position %d with position %d",&a,&b)==2)swapPos(pwd,a,b);
            else if(sscanf(op,"swap letter %s with letter %s",s1,s2)==2)swapLet(pwd,s1[0],s2[0]);
            else if(sscanf(op,"rotate left %d",&a)==1)rotL(pwd,a%strlen(pwd),strlen(pwd));
            else if(sscanf(op,"rotate right %d",&a)==1)rotR(pwd,a%strlen(pwd),strlen(pwd));
            else if(sscanf(op,"rotate based on position of letter %s",s1)==1)rotBased(pwd,s1[0]);
            else if(sscanf(op,"reverse positions %d through %d",&a,&b)==2)revPos(pwd,a,b);
            else if(sscanf(op,"move position %d to position %d",&a,&b)==2)movePos(pwd,a,b);
        }
        fclose(f);
        printf("%s\n",pwd);
    }
    return 0;
}
