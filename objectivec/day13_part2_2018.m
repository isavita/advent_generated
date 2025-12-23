#import <Foundation/Foundation.h>

@interface CartObj : NSObject
@property (nonatomic) int x,y,turns;
@property (nonatomic) char direction;
@end
@implementation CartObj @end

void moveCart(CartObj *cart, NSArray<NSMutableString *> *tracks){
    switch (cart.direction) {
        case '>': cart.x++; break;
        case '<': cart.x--; break;
        case '^': cart.y--; break;
        case 'v': cart.y++; break;
    }
    unichar trackChar = [tracks[cart.y] characterAtIndex:cart.x];
    switch (trackChar) {
        case '+':
            if (cart.turns % 3 == 0) {
                cart.direction = (cart.direction=='>'? '^' : (cart.direction=='<'? 'v' : (cart.direction=='^'? '<' : '>')));
            } else if (cart.turns % 3 == 2) {
                cart.direction = (cart.direction=='>'? 'v' : (cart.direction=='<'? '^' : (cart.direction=='^'? '>' : '<')));
            }
            cart.turns++;
            break;
        case '/':
            cart.direction = (cart.direction=='>'? '^' : (cart.direction=='<'? 'v' : (cart.direction=='^'? '>' : '<')));
            break;
        case '\\':
            cart.direction = (cart.direction=='>'? 'v' : (cart.direction=='<'? '^' : (cart.direction=='^'? '<' : '>')));
            break;
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *inputPath = @"input.txt";
        NSError *error = nil;
        NSString *content = [NSString stringWithContentsOfFile:inputPath encoding:NSUTF8StringEncoding error:&error];
        if (!content) return 1;
        NSArray<NSString *> *rawLines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray<NSMutableString *> *tracks = [NSMutableArray array];
        NSMutableArray<CartObj *> *carts = [NSMutableArray array];
        for (int y = 0; y < rawLines.count; y++) {
            NSString *line = rawLines[y];
            if (line.length==0 && y==rawLines.count-1) continue;
            NSMutableString *mutableLine = [line mutableCopy];
            for (int x = 0; x < mutableLine.length; x++) {
                unichar ch = [mutableLine characterAtIndex:x];
                if (ch=='>'||ch=='<'||ch=='^'||ch=='v') {
                    CartObj *c = [[CartObj alloc] init];
                    c.x = x; c.y = y; c.direction = (char)ch; c.turns = 0;
                    [carts addObject:c];
                    [mutableLine replaceCharactersInRange:NSMakeRange(x,1) withString:(ch=='>'||ch=='<'?@"-" : @"|")];
                }
            }
            [tracks addObject:mutableLine];
        }
        while (carts.count > 1) {
            NSArray *sortedCarts = [carts sortedArrayUsingComparator:^NSComparisonResult(CartObj *a, CartObj *b){
                if (a.y != b.y) return a.y < b.y? NSOrderedAscending : NSOrderedDescending;
                return a.x < b.x? NSOrderedAscending : NSOrderedDescending;
            }];
            carts = [sortedCarts mutableCopy];
            NSMutableSet *toRemove = [NSMutableSet set];
            for (int i = 0; i < carts.count; i++) {
                if ([toRemove containsObject:@(i)]) continue;
                moveCart(carts[i], tracks);
                for (int j = 0; j < carts.count; j++) {
                    if (i==j || [toRemove containsObject:@(j)]) continue;
                    if (carts[i].x==carts[j].x && carts[i].y==carts[j].y) {
                        [toRemove addObject:@(i)];
                        [toRemove addObject:@(j)];
                    }
                }
            }
            NSMutableArray *newCarts = [NSMutableArray array];
            for (int i = 0; i < carts.count; i++) {
                if (![toRemove containsObject:@(i)]) [newCarts addObject:carts[i]];
            }
            carts = newCarts;
        }
        if (carts.count==1) {
            CartObj *last = carts[0];
            printf("%d,%d\n", last.x, last.y);
        }
    }
    return 0;
}
