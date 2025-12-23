
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

#define MAX_MAP_SIZE 100
#define MAX_UNITS 100

typedef enum {
    KindSpace = 1, KindElf = 2, KindGoblin = 4, KindWall = 8
} Kind;

typedef struct Coordinate {
    int x, y;
} Coordinate;

typedef struct Tile Tile;
typedef struct Unit Unit;

struct Tile {
    Kind kind;
    int x, y;
    Unit *unit;
    Tile* neighbors[4];
};

struct Unit {
    Kind kind;
    int hitpoints;
    int power;
    Tile *tile;
};

typedef struct {
    Tile* tiles[MAX_MAP_SIZE][MAX_MAP_SIZE];
    int rows, cols;
} Map;

typedef struct {
    Unit* units[MAX_UNITS];
    int count;
} UnitList;

typedef struct {
    Map map;
    UnitList units;
} Cave;

const Coordinate offsets[] = {{0,-1},{-1,0},{1,0},{0,1}};
const int defaultHitpoints = 200;
const int defaultPower = 3;

bool isUnit(Kind k){ return (k==KindElf||k==KindGoblin); }

void initCave(Cave* c){ c->units.count=0; for(int y=0;y<MAX_MAP_SIZE;y++)for(int x=0;x<MAX_MAP_SIZE;x++)c->map.tiles[y][x]=NULL; c->map.rows=0;c->map.cols=0; }

Unit* newUnit(Tile* t, Kind k, int p){ Unit*u=malloc(sizeof(Unit));u->kind=k;u->hitpoints=defaultHitpoints;u->power=defaultPower;u->tile=t;t->unit=u;if(k==KindElf)u->power=p;return u;}

void parseMap(Cave* c,char** inp,int p){int y=0;while(inp[y]){int len=strlen(inp[y]);if(len==0)break;if(c->map.cols==0)c->map.cols=len;for(int x=0;x<len;x++){char cch=inp[y][x];Kind k;if(cch=='.')k=KindSpace;else if(cch=='E')k=KindElf;else if(cch=='G')k=KindGoblin;else k=KindWall;Tile*t=malloc(sizeof(Tile));t->kind=k;t->x=x;t->y=y;t->unit=NULL;c->map.tiles[y][x]=t;if(isUnit(k)){Unit*u=newUnit(t,k,p);c->units.units[c->units.count++]=u;}}y++;}c->map.rows=y;for(int y=0;y<c->map.rows;y++)for(int x=0;x<c->map.cols;x++){Tile*t=c->map.tiles[y][x];if(t){for(int i=0;i<4;i++){int nx=x+offsets[i].x,ny=y+offsets[i].y; t->neighbors[i]=(nx>=0&&nx<c->map.cols&&ny>=0&&ny<c->map.rows)?c->map.tiles[ny][nx]:NULL;}}}}

void freeCave(Cave* c){for(int y=0;y<c->map.rows;y++)for(int x=0;x<c->map.cols;x++)if(c->map.tiles[y][x])free(c->map.tiles[y][x]);for(int i=0;i<c->units.count;i++)free(c->units.units[i]);}

int compareUnits(const void*a,const void*b){Unit*u=* (Unit**)a, *v=* (Unit**)b;return u->tile->y==v->tile->y?u->tile->x-v->tile->x:u->tile->y-v->tile->y;}
void sortUnits(UnitList*u){qsort(u->units,u->count,sizeof(Unit*),compareUnits);}

int status(Cave* c,bool* still){bool e=0,g=0;int hp=0;for(int i=0;i<c->units.count;i++){Unit*u=c->units.units[i];if(u->hitpoints<=0)continue;e|=u->kind==KindElf;g|=u->kind==KindGoblin;hp+=u->hitpoints;}*still=e&&g;return hp;}
void removeTheDead(Cave*c){int n=0;for(int i=0;i<c->units.count;i++)if(c->units.units[i]->hitpoints>0)c->units.units[n++]=c->units.units[i];c->units.count=n;}

void removeUnit(Cave*c,Unit*u){u->tile->kind=KindSpace;u->tile->unit=NULL;u->tile=NULL;}

typedef struct{Tile*t;int d;}QueueItem;
#define QSIZE (MAX_MAP_SIZE*MAX_MAP_SIZE)
typedef struct{QueueItem a[QSIZE];int h,t;}Queue;
void initQ(Queue*q){q->h=q->t=0;}
bool isQEmpty(Queue*q){return q->h==q->t;}
void enqueue(Queue*q,Tile*t,int d){q->a[q->t].t=t;q->a[q->t].d=d; q->t=(q->t+1)%QSIZE;}
QueueItem dequeue(Queue*q){QueueItem r=q->a[q->h];q->h=(q->h+1)%QSIZE;return r;}

typedef struct{int dist[MAX_MAP_SIZE][MAX_MAP_SIZE];Tile* from[MAX_MAP_SIZE][MAX_MAP_SIZE];int ok;}Dist;
void initD(Dist*d,Cave*c){d->ok=0;for(int y=0;y<c->map.rows;y++)for(int x=0;x<c->map.cols;x++){d->dist[y][x]=-1;d->from[y][x]=NULL;}}
void walk(Cave*c,Tile*s,Dist*d){initD(d,c);Queue q;initQ(&q);enqueue(&q,s,0);d->dist[s->y][s->x]=0;d->ok=1;while(!isQEmpty(&q)){QueueItem qi=dequeue(&q);Tile*t=qi.t;int d0=qi.d;for(int i=0;i<4;i++){Tile*nb=t->neighbors[i];if(nb&&nb->kind==KindSpace&&d->dist[nb->y][nb->x]==-1){enqueue(&q,nb,d0+1);d->dist[nb->y][nb->x]=d0+1;d->from[nb->y][nb->x]=t;}}}}

bool targets(Unit*u,Cave*c){for(int i=0;i<c->units.count;i++){Unit*v=c->units.units[i];if(v->kind!=u->kind&&v->hitpoints>0)return 1;}return 0;}
Tile* nextTile(Unit*u,Cave*c,Dist*d,Tile** ttarget){int best=INT_MAX;Tile* target=NULL;Tile* next=NULL;for(int i=0;i<c->units.count;i++){Unit*v=c->units.units[i];if(v->kind==u->kind||v->hitpoints<=0)continue;for(int j=0;j<4;j++){Tile*n=v->tile->neighbors[j];if(n&&d->dist[n->y][n->x]!=-1&&d->dist[n->y][n->x]<=best){if(d->dist[n->y][n->x]<best){best=d->dist[n->y][n->x];target=n;}else if(n->y<target->y||n->y==target->y&&n->x<target->x)target=n;}}}if(target){*ttarget=target;Tile*cst=target;while(d->from[cst->y][cst->x]!=u->tile){cst=d->from[cst->y][cst->x];if(!cst)return NULL;}next=cst;}return next;}
Unit* enemyNear(Unit*u,Cave*c){Unit* best=NULL;for(int i=0;i<4;i++){Tile*n=u->tile->neighbors[i];if(n&&n->unit&&n->unit->kind!=u->kind&&n->unit->hitpoints>0){if(!best||n->unit->hitpoints<best->hitpoints)best=n->unit;}}return best;}
void move(Unit*u,Cave*c,Dist*d){if(enemyNear(u,c))return;Tile* t;Tile* n=nextTile(u,c,d,&t);if(n){n->unit=u;n->kind=u->kind;u->tile->kind=KindSpace;u->tile->unit=NULL;u->tile=n;}}
bool attack(Unit*u,Cave*c){Unit* e=enemyNear(u,c);if(e){e->hitpoints-=u->power;if(e->hitpoints<=0){removeUnit(c,e);return e->kind==KindElf;}}return 0;}
bool tick(Cave*c,bool stop,Dist*d){removeTheDead(c);sortUnits(&c->units);for(int i=0;i<c->units.count;i++){Unit*u=c->units.units[i];if(u->hitpoints<=0)continue;if(!targets(u,c))return 0;walk(c,u->tile,d);move(u,c,d);if(attack(u,c)&&stop)return 0;}return 1;}
int combat(char** inp){Cave c;initCave(&c);parseMap(&c,inp,defaultPower);Dist d;for(int r=1;;r++){bool fight;int hp=status(&c,&fight);if(!fight){int res=(r-1)*hp;freeCave(&c);return res;}if(!tick(&c,false,&d))r--;}}
int main(){FILE*f=fopen("input.txt","r");if(!f){perror("open");return 1;}char* lines[MAX_MAP_SIZE];char buf[1024];int l=0;while(fgets(buf,sizeof(buf),f)){lines[l]=strdup(buf);if(!lines[l]){perror("alloc");return 1;}size_t len=strlen(lines[l]);if(len&&lines[l][len-1]=='\n')lines[l][len-1]='\0';if(len==1&&lines[l][0]=='\0'){free(lines[l]);lines[l]=NULL;break;}l++;}lines[l]=NULL;fclose(f);int ans=combat(lines);printf("%d\n",ans);for(int i=0;lines[i];i++)free(lines[i]);return 0;}
