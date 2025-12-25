
#import <Foundation/Foundation.h>

@interface Nanobot : NSObject
@property int x, y, z, r;
- (instancetype)initWithX:(int)x Y:(int)y Z:(int)z R:(int)r;
@end
@implementation Nanobot
- (instancetype)initWithX:(int)x Y:(int)y Z:(int)z R:(int)r {
    if (self = [super init]) { _x=x; _y=y; _z=z; _r=r; } return self;
}
@end

@interface CubeObj : NSObject
@property int count, distance, size, x, y, z;
- (instancetype)initWithCount:(int)c distance:(int)d size:(int)s X:(int)x Y:(int)y Z:(int)z;
@end
@implementation CubeObj
- (instancetype)initWithCount:(int)c distance:(int)d size:(int)s X:(int)x Y:(int)y Z:(int)z {
    if (self = [super init]) { _count=c; _distance=d; _size=s; _x=x; _y=y; _z=z; } return self;
}
@end

int manhattan(int x1,int y1,int z1,int x2,int y2,int z2){return abs(x1-x2)+abs(y1-y2)+abs(z1-z2);}
int minDistOrigin(int x,int y,int z,int s){
    int dx = (x>0)?x:((x+s-1<0)?-(x+s-1):0);
    int dy = (y>0)?y:((y+s-1<0)?-(y+s-1):0);
    int dz = (z>0)?z:((z+s-1<0)?-(z+s-1):0);
    return dx+dy+dz;
}
NSMutableArray<CubeObj *> *heap;
void heapSwap(int i,int j){[heap exchangeObjectAtIndex:i withObjectAtIndex:j];}
int heapCmp(CubeObj *a, CubeObj *b){
    if(a.count!=b.count) return b.count-a.count;
    return a.distance-b.distance;
}
void heapPush(CubeObj *c){
    [heap addObject:c];
    int i=heap.count-1;
    while(i>0){
        int p=(i-1)/2;
        if(heapCmp(heap[p],heap[i])<=0)break;
        heapSwap(i,p); i=p;
    }
}
CubeObj *heapPop(){
    CubeObj *top=heap[0];
    CubeObj *last=heap.lastObject;
    heap[0]=last;
    [heap removeLastObject];
    int i=0;
    while(1){
        int l=i*2+1,r=l+1,small=i;
        if(l<heap.count && heapCmp(heap[l],heap[small])<0) small=l;
        if(r<heap.count && heapCmp(heap[r],heap[small])<0) small=r;
        if(small==i)break;
        heapSwap(i,small); i=small;
    }
    return top;
}

NSArray<Nanobot *> *parseInput(){
    NSString *data=[NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
    NSArray *lines=[data componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
    NSRegularExpression *re=[NSRegularExpression regularExpressionWithPattern:@"pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)" options:0 error:nil];
    NSMutableArray *arr=[NSMutableArray array];
    for(NSString *line in lines){
        NSTextCheckingResult *m=[re firstMatchInString:line options:0 range:NSMakeRange(0,line.length)];
        if(m){
            int x=[[line substringWithRange:[m rangeAtIndex:1]] intValue];
            int y=[[line substringWithRange:[m rangeAtIndex:2]] intValue];
            int z=[[line substringWithRange:[m rangeAtIndex:3]] intValue];
            int r=[[line substringWithRange:[m rangeAtIndex:4]] intValue];
            [arr addObject:[[Nanobot alloc] initWithX:x Y:y Z:z R:r]];
        }
    }
    return arr;
}

int partOne(NSArray<Nanobot *> *bots){
    Nanobot *strong=nil;
    for(Nanobot *b in bots) if(!strong||b.r>strong.r) strong=b;
    int cnt=0;
    for(Nanobot *b in bots) if(manhattan(strong.x,strong.y,strong.z,b.x,b.y,b.z)<=strong.r) cnt++;
    return cnt;
}

int partTwo(NSArray<Nanobot *> *bots){
    int minX=INT_MAX,maxX=INT_MIN,minY=INT_MAX,maxY=INT_MIN,minZ=INT_MAX,maxZ=INT_MIN;
    for(Nanobot *b in bots){
        if(b.x<minX)minX=b.x; if(b.x>maxX)maxX=b.x;
        if(b.y<minY)minY=b.y; if(b.y>maxY)maxY=b.y;
        if(b.z<minZ)minZ=b.z; if(b.z>maxZ)maxZ=b.z;
    }
    int size=1;
    int range=MAX(maxX-minX,MAX(maxY-minY,maxZ-minZ));
    while(size<range)size<<=1;
    heap=[NSMutableArray array];
    heapPush([[CubeObj alloc] initWithCount:0 distance:minDistOrigin(minX,minY,minZ,size) size:size X:minX Y:minY Z:minZ]);
    int bestDist=0,bestCnt=-1;
    while(heap.count){
        CubeObj *c=heapPop();
        if(c.size==1){
            if(c.count>bestCnt|| (c.count==bestCnt&&c.distance<bestDist)){
                bestCnt=c.count; bestDist=c.distance;
                break;
            }
            continue;
        }
        int half=c.size/2;
        for(int dx=0;dx<=half;dx+=half)
        for(int dy=0;dy<=half;dy+=half)
        for(int dz=0;dz<=half;dz+=half){
            int nx=c.x+dx, ny=c.y+dy, nz=c.z+dz;
            int newSize=MAX(1,half);
            int cnt=0;
            for(Nanobot *b in bots){
                int d=0;
                if(b.x<nx) d+=nx-b.x; else if(b.x>nx+newSize-1) d+=b.x-(nx+newSize-1);
                if(b.y<ny) d+=ny-b.y; else if(b.y>ny+newSize-1) d+=b.y-(ny+newSize-1);
                if(b.z<nz) d+=nz-b.z; else if(b.z>nz+newSize-1) d+=b.z-(nz+newSize-1);
                if(d<=b.r) cnt++;
            }
            int dist=minDistOrigin(nx,ny,nz,newSize);
            heapPush([[CubeObj alloc] initWithCount:cnt distance:dist size:newSize X:nx Y:ny Z:nz]);
        }
    }
    return bestDist;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<Nanobot *> *bots=parseInput();
        printf("%d\n",partOne(bots));
        printf("%d\n",partTwo(bots));
    }
    return 0;
}
