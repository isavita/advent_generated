
#import <Foundation/Foundation.h>

typedef struct {int x; int y;} P;

@interface Elf : NSObject
@property (assign) P pos;
@property (assign) P nextPos;
@property (assign) BOOL moving;
- (instancetype)initWithPos:(P)p;
@end

@implementation Elf
- (instancetype)initWithPos:(P)p { if (self=[super init]){_pos=p;_moving=NO;} return self; }
@end

static const int N=1, E=3, S=5, W=7;
static const int order[4]={N,S,W,E};
static const P dirs[8]={{-1,-1},{-1,0},{-1,1},{0,1},{1,1},{1,0},{1,-1},{0,-1}};
static NSMutableDictionary<NSString*,NSNumber*> *map;
static NSMutableArray<Elf*> *elves;
static int currDir=0;

static NSString* key(P p){ return [NSString stringWithFormat:@"%d,%d",p.x,p.y]; }

static BOOL aroundAllEmpty(Elf *e){
    for(int i=0;i<8;i++){
        P a={e.pos.x+dirs[i].x, e.pos.y+dirs[i].y};
        if(map[key(a)]) return NO;
    }
    return YES;
}

static BOOL elfInDirection(Elf *e,int dir){
    for(int j=-1;j<=1;j++){
        int idx=(dir+j+8)%8;
        P a={e.pos.x+dirs[idx].x, e.pos.y+dirs[idx].y};
        if(map[key(a)]) return YES;
    }
    return NO;
}

static BOOL run(){
    NSMutableDictionary<NSString*,NSNumber*> *proposes=[NSMutableDictionary dictionary];
    for(Elf *e in elves){
        e.moving=NO;
        if(aroundAllEmpty(e)) continue;
        for(int i=0;i<4;i++){
            int dir=order[(currDir+i)%4];
            if(elfInDirection(e,dir)) continue;
            P d=dirs[dir];
            P dest={e.pos.x+d.x, e.pos.y+d.y};
            NSString *k=key(dest);
            proposes[k]=@([proposes[k] intValue]+1);
            e.nextPos=dest;
            e.moving=YES;
            break;
        }
    }
    BOOL moved=NO;
    for(Elf *e in elves){
        if(!e.moving) continue;
        NSString *k=key(e.nextPos);
        if([proposes[k] intValue]>1){
            e.moving=NO;
            continue;
        }
        moved=YES;
        [map removeObjectForKey:key(e.pos)];
        map[k]=@YES;
        e.pos=e.nextPos;
        e.moving=NO;
    }
    currDir=(currDir+1)%4;
    return moved;
}

static void minMax(P *min,P *max){
    min->x=INT_MAX; min->y=INT_MAX;
    max->x=INT_MIN; max->y=INT_MIN;
    for(NSString *k in map){
        NSArray *parts=[k componentsSeparatedByString:@","];
        int x=[parts[0] intValue], y=[parts[1] intValue];
        if(x<min->x) min->x=x;
        if(y<min->y) min->y=y;
        if(x>max->x) max->x=x;
        if(y>max->y) max->y=y;
    }
}

static void parse(){
    map=[NSMutableDictionary dictionary];
    elves=[NSMutableArray array];
    NSString *content=[NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
    NSArray<NSString*> *lines=[content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
    int row=0;
    for(NSString *line in lines){
        for(int col=0;col<line.length;col++){
            if([line characterAtIndex:col]== '#'){
                P p={row,col};
                map[key(p)]=@YES;
                [elves addObject:[[Elf alloc] initWithPos:p]];
            }
        }
        row++;
    }
}

int main(int argc, const char * argv[]){
    @autoreleasepool{
        parse();
        for(int i=0;i<10;i++) run();
        P min,max;
        minMax(&min,&max);
        int count=0;
        for(int x=min.x;x<=max.x;x++){
            for(int y=min.y;y<=max.y;y++){
                if(!map[key((P){x,y})]) count++;
            }
        }
        printf("%d\n",count);
    }
    return 0;
}
