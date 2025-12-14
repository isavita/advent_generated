
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {int x,y,z;} Point;
typedef struct {int u,v; long long d;} Edge;

static long long distSq(const Point *a, const Point *b) {
    long long dx=a->x-b->x, dy=a->y-b->y, dz=a->z-b->z;
    return dx*dx+dy*dy+dz*dz;
}
static int cmpEdge(const void *a,const void *b){
    long long da=((Edge*)a)->d, db=((Edge*)b)->d;
    return (da>db)-(da<db);
}
static int find(int *p,int x){
    while(p[x]!=x){p[x]=p[p[x]];x=p[x];}
    return x;
}
static void unite(int *p,int *r,int a,int b){
    a=find(p,a);b=find(p,b);
    if(a==b)return;
    if(r[a]<r[b])p[a]=b;
    else if(r[a]>r[b])p[b]=a;
    else{p[b]=a;r[a]++;}
}

int main(void){
    FILE *f=fopen("input.txt","r");
    if(!f)return 1;
    Point *pts=NULL;
    size_t n=0,cap=0;
    char line[256];
    while(fgets(line,sizeof line,f)){
        char *p=line;
        while(*p&&(*p==' '||*p=='\n'||*p=='\r'))p++;
        if(!*p)continue;
        int x,y,z;
        if(sscanf(p,"%d,%d,%d",&x,&y,&z)!=3)continue;
        if(n==cap){cap=cap?cap*2:64;pts=realloc(pts,cap*sizeof(Point));}
        pts[n++] = (Point){x,y,z};
    }
    fclose(f);
    if(n<2)return 0;
    size_t ecap=n*(n-1)/2;
    Edge *edges=malloc(ecap*sizeof(Edge));
    size_t e=0;
    for(size_t i=0;i<n;i++)for(size_t j=i+1;j<n;j++)
        edges[e++] = (Edge){(int)i,(int)j,distSq(&pts[i],&pts[j])};
    qsort(edges,e,sizeof(Edge),cmpEdge);
    int *parent=malloc(n*sizeof(int));
    int *rank=calloc(n,sizeof(int));
    for(size_t i=0;i<n;i++)parent[i]=i;
    size_t comps=n;
    for(size_t i=0;i<e;i++){
        int u=edges[i].u, v=edges[i].v;
        int ru=find(parent,u), rv=find(parent,v);
        if(ru!=rv){
            unite(parent,rank,ru,rv);
            if(--comps==1){
                Point p1=pts[u], p2=pts[v];
                printf("Connected %d,%d,%d and %d,%d,%d\n",p1.x,p1.y,p1.z,p2.x,p2.y,p2.z);
                printf("Product of X coordinates: %lld\n",(long long)p1.x*p2.x);
                break;
            }
        }
    }
    free(pts);free(edges);free(parent);free(rank);
    return 0;
}
