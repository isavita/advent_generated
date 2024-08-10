#import <Foundation/Foundation.h>

@interface Packet : NSObject
@property (nonatomic, strong) NSArray *data;
- (instancetype)initWithString:(NSString *)string;
- (NSComparisonResult)compare:(Packet *)otherPacket;
@end

@implementation Packet

- (instancetype)initWithString:(NSString *)string {
    self = [super init];
    if (self) {
        NSData *data = [string dataUsingEncoding:NSUTF8StringEncoding];
        id json = [NSJSONSerialization JSONObjectWithData:data options:0 error:nil];
        if ([json isKindOfClass:[NSArray class]]) {
            _data = json;
        } else {
            _data = @[json];
        }
    }
    return self;
}

- (NSComparisonResult)compare:(Packet *)otherPacket {
    return [self compareArray:_data withArray:otherPacket.data];
}

- (NSComparisonResult)compareArray:(NSArray *)left withArray:(NSArray *)right {
    NSUInteger length = MIN(left.count, right.count);
    for (NSUInteger i = 0; i < length; i++) {
        id leftItem = left[i];
        id rightItem = right[i];
        if ([leftItem isKindOfClass:[NSNumber class]] && [rightItem isKindOfClass:[NSNumber class]]) {
            if ([leftItem integerValue] < [rightItem integerValue]) {
                return NSOrderedAscending;
            } else if ([leftItem integerValue] > [rightItem integerValue]) {
                return NSOrderedDescending;
            }
        } else {
            NSArray *leftArray = [leftItem isKindOfClass:[NSNumber class]] ? @[leftItem] : leftItem;
            NSArray *rightArray = [rightItem isKindOfClass:[NSNumber class]] ? @[rightItem] : rightItem;
            NSComparisonResult result = [self compareArray:leftArray withArray:rightArray];
            if (result != NSOrderedSame) {
                return result;
            }
        }
    }
    if (left.count < right.count) {
        return NSOrderedAscending;
    } else if (left.count > right.count) {
        return NSOrderedDescending;
    }
    return NSOrderedSame;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];

        NSMutableArray *packets = [NSMutableArray array];
        NSMutableArray *allPackets = [NSMutableArray array];
        int indexSum = 0;
        int pairIndex = 1;

        Packet *dividerPacket1 = [[Packet alloc] initWithString:@"[[2]]"];
        Packet *dividerPacket2 = [[Packet alloc] initWithString:@"[[6]]"];
        [allPackets addObject:dividerPacket1];
        [allPackets addObject:dividerPacket2];

        for (NSUInteger i = 0; i < lines.count; i += 3) {
            Packet *leftPacket = [[Packet alloc] initWithString:lines[i]];
            Packet *rightPacket = [[Packet alloc] initWithString:lines[i + 1]];
            [allPackets addObject:leftPacket];
            [allPackets addObject:rightPacket];
            [packets addObject:@[leftPacket, rightPacket]];

            if ([leftPacket compare:rightPacket] == NSOrderedAscending) {
                indexSum += pairIndex;
            }
            pairIndex++;
        }

        NSLog(@"Sum of indices of pairs in the right order: %d", indexSum);

        [allPackets sortUsingComparator:^NSComparisonResult(Packet *p1, Packet *p2) {
            return [p1 compare:p2];
        }];

        int decoderKey = 1;
        for (NSUInteger i = 0; i < allPackets.count; i++) {
            Packet *packet = allPackets[i];
            if ([packet.data isEqualToArray:dividerPacket1.data] || [packet.data isEqualToArray:dividerPacket2.data]) {
                decoderKey *= (i + 1);
            }
        }

        NSLog(@"Decoder key for the distress signal: %d", decoderKey);
    }
    return 0;
}