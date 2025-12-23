
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef void (*op_func)(int *, int, int, int);

static void addr(int *r,int a,int b,int c){ r[c]=r[a]+r[b]; }
static void addi(int *r,int a,int b,int c){ r[c]=r[a]+b; }
static void mulr(int *r,int a,int b,int c){ r[c]=r[a]*r[b]; }
static void muli(int *r,int a,int b,int c){ r[c]=r[a]*b; }
static void banr(int *r,int a,int b,int c){ r[c]=r[a]&r[b]; }
static void bani(int *r,int a,int b,int c){ r[c]=r[a]&b; }
static void borr(int *r,int a,int b,int c){ r[c]=r[a]|r[b]; }
static void bori(int *r,int a,int b,int c){ r[c]=r[a]|b; }
static void setr(int *r,int a,int b,int c){ r[c]=r[a]; }
static void seti(int *r,int a,int b,int c){ r[c]=a; }
static void gtir(int *r,int a,int b,int c){ r[c]=(a>r[b])?1:0; }
static void gtri(int *r,int a,int b,int c){ r[c]=(r[a]>b)?1:0; }
static void gtrr(int *r,int a,int b,int c){ r[c]=(r[a]>r[b])?1:0; }
static void eqir(int *r,int a,int b,int c){ r[c]=(a==r[b])?1:0; }
static void eqri(int *r,int a,int b,int c){ r[c]=(r[a]==b)?1:0; }
static void eqrr(int *r,int a,int b,int c){ r[c]=(r[a]==r[b])?1:0; }

static op_func ops[16] = {addr,addi,mulr,muli,banr,bani,borr,bori,
                          setr,seti,gtir,gtri,gtrr,eqir,eqri,eqrr};

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *fp = fopen("input.txt","r");
        if (!fp) { perror("Error opening file"); return 1; }

        int before[4], after[4], inst[4];
        int three_or_more = 0;
        int possible_ops[16][16];
        memset(possible_ops,1,sizeof(possible_ops));

        while (fscanf(fp,"Before: [%d, %d, %d, %d]\n",
                      &before[0],&before[1],&before[2],&before[3])==4) {
            fscanf(fp,"%d %d %d %d\n",&inst[0],&inst[1],&inst[2],&inst[3]);
            fscanf(fp,"After:  [%d, %d, %d, %d]\n\n",
                      &after[0],&after[1],&after[2],&after[3]);

            int matching_ops=0;
            for (int i=0;i<16;i++){
                int temp[4];
                memcpy(temp,before,sizeof(before));
                ops[i](temp,inst[1],inst[2],inst[3]);
                if (memcmp(temp,after,sizeof(after))==0) matching_ops++; else
                    possible_ops[inst[0]][i]=0;
            }
            if (matching_ops>=3) three_or_more++;
        }
        printf("Part 1: %d\n",three_or_more);

        int op_map[16];
        for (int i=0;i<16;i++) op_map[i]=-1;
        for (int i=0;i<16;i++){
            for (int j=0;j<16;j++){
                int count=0,index=-1;
                for (int k=0;k<16;k++) if (possible_ops[j][k]){ count++; index=k; }
                if (count==1){
                    op_map[j]=index;
                    for (int l=0;l<16;l++) possible_ops[l][index]=0;
                }
            }
        }

        int reg[4]={0};
        while (fscanf(fp,"%d %d %d %d\n",&inst[0],&inst[1],&inst[2],&inst[3])==4){
            ops[op_map[inst[0]]](reg,inst[1],inst[2],inst[3]);
        }
        printf("Part 2: %d\n",reg[0]);

        fclose(fp);
        return 0;
    }
}
