
#import <Foundation/Foundation.h>

NSString *hexToBin(NSString *hex) {
    NSMutableString *bin = [NSMutableString string];
    for (int i = 0; i < hex.length; i++) {
        unichar h = [hex characterAtIndex:i];
        unsigned char b = 0;
        if (h >= '0' && h <= '9') b = h - '0';
        else b = 10 + (h - 'A');
        [bin appendFormat:@"%04d", (b&8?1:0)*1000+(b&4?1:0)*100+(b&2?1:0)*10+(b&1?1:0)];
    }
    return bin;
}

int parsePacket(NSString *binStr, int idx, int *newIdx, long long *value) {
    int version = (([binStr characterAtIndex:idx]-'0')<<2) |
                  (([binStr characterAtIndex:idx+1]-'0')<<1) |
                  ([binStr characterAtIndex:idx+2]-'0');
    int typeID = (([binStr characterAtIndex:idx+3]-'0')<<2) |
                 (([binStr characterAtIndex:idx+4]-'0')<<1) |
                 ([binStr characterAtIndex:idx+5]-'0');
    idx += 6;

    if (typeID == 4) {
        long long val = 0;
        while ([binStr characterAtIndex:idx] == '1') {
            val = (val<<4) |
                  (([binStr characterAtIndex:idx+1]-'0')<<3) |
                  (([binStr characterAtIndex:idx+2]-'0')<<2) |
                  (([binStr characterAtIndex:idx+3]-'0')<<1) |
                  ([binStr characterAtIndex:idx+4]-'0');
            idx += 5;
        }
        val = (val<<4) |
              (([binStr characterAtIndex:idx+1]-'0')<<3) |
              (([binStr characterAtIndex:idx+2]-'0')<<2) |
              (([binStr characterAtIndex:idx+3]-'0')<<1) |
              ([binStr characterAtIndex:idx+4]-'0');
        idx += 5;
        *value = val;
        *newIdx = idx;
        return version;
    }

    int lengthTypeID = [binStr characterAtIndex:idx++] - '0';
    int numSubPackets = 0, subPacketLength = 0;
    if (lengthTypeID == 0) {
        for (int i = 0; i < 15; i++)
            subPacketLength = (subPacketLength<<1) | ([binStr characterAtIndex:idx++] - '0');
    } else {
        for (int i = 0; i < 11; i++)
            numSubPackets = (numSubPackets<<1) | ([binStr characterAtIndex:idx++] - '0');
    }

    NSMutableArray *values = [NSMutableArray array];
    while (1) {
        if (lengthTypeID == 0 && subPacketLength == 0) break;
        if (lengthTypeID == 1 && numSubPackets == 0) break;
        int nidx; long long v;
        version += parsePacket(binStr, idx, &nidx, &v);
        [values addObject:@(v)];
        if (lengthTypeID == 0) subPacketLength -= nidx - idx;
        else numSubPackets--;
        idx = nidx;
    }

    long long result = 0;
    switch (typeID) {
        case 0: for (NSNumber *n in values) result += [n longLongValue]; break;
        case 1: result = 1; for (NSNumber *n in values) result *= [n longLongValue]; break;
        case 2: result = [values[0] longLongValue]; for (NSNumber *n in values) if ([n longLongValue] < result) result = [n longLongValue]; break;
        case 3: result = [values[0] longLongValue]; for (NSNumber *n in values) if ([n longLongValue] > result) result = [n longLongValue]; break;
        case 5: result = ([values[0] longLongValue] > [values[1] longLongValue]) ? 1 : 0; break;
        case 6: result = ([values[0] longLongValue] < [values[1] longLongValue]) ? 1 : 0; break;
        case 7: result = ([values[0] longLongValue] == [values[1] longLongValue]) ? 1 : 0; break;
    }
    *value = result;
    *newIdx = idx;
    return version;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *hex = [[[NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet] uppercaseString];
        NSString *bin = hexToBin(hex);
        int n; long long value;
        parsePacket(bin, 0, &n, &value);
        printf("%lld\n", value);
    }
    return 0;
}
