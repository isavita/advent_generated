
#import <Foundation/Foundation.h>

static void hexToBin(NSString *hex, NSMutableString *bin) {
    for (NSUInteger i = 0; i < hex.length; i++) {
        unichar c = [hex characterAtIndex:i];
        switch (c) {
            case '0': [bin appendString:@"0000"]; break;
            case '1': [bin appendString:@"0001"]; break;
            case '2': [bin appendString:@"0010"]; break;
            case '3': [bin appendString:@"0011"]; break;
            case '4': [bin appendString:@"0100"]; break;
            case '5': [bin appendString:@"0101"]; break;
            case '6': [bin appendString:@"0110"]; break;
            case '7': [bin appendString:@"0111"]; break;
            case '8': [bin appendString:@"1000"]; break;
            case '9': [bin appendString:@"1001"]; break;
            case 'A': case 'a': [bin appendString:@"1010"]; break;
            case 'B': case 'b': [bin appendString:@"1011"]; break;
            case 'C': case 'c': [bin appendString:@"1100"]; break;
            case 'D': case 'd': [bin appendString:@"1101"]; break;
            case 'E': case 'e': [bin appendString:@"1110"]; break;
            case 'F': case 'f': [bin appendString:@"1111"]; break;
        }
    }
}

static int parsePacket(const char *b, int idx, int *sum) {
    int v = (b[idx]-'0')<<2 | (b[idx+1]-'0')<<1 | (b[idx+2]-'0');
    int t = (b[idx+3]-'0')<<2 | (b[idx+4]-'0')<<1 | (b[idx+5]-'0');
    idx += 6;
    *sum += v;
    if (t == 4) {
        while (b[idx] == '1') idx += 5;
        idx += 5;
        return idx;
    }
    int lt = b[idx++] - '0';
    int len = 0, cnt = 0;
    if (lt == 0) {
        for (int i = 0; i < 15; i++) len = (len<<1) | (b[idx++]-'0');
    } else {
        for (int i = 0; i < 11; i++) cnt = (cnt<<1) | (b[idx++]-'0');
    }
    if (lt == 0) {
        int start = idx;
        while (idx - start < len) idx = parsePacket(b, idx, sum);
    } else {
        for (int i = 0; i < cnt; i++) idx = parsePacket(b, idx, sum);
    }
    return idx;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *hex = [NSString stringWithContentsOfFile:@"input.txt"
                                                  encoding:NSUTF8StringEncoding
                                                     error:nil];
        hex = [hex stringByTrimmingCharactersInSet:
               [NSCharacterSet whitespaceAndNewlineCharacterSet]];
        NSMutableString *bin = [NSMutableString stringWithCapacity:hex.length*4];
        hexToBin(hex, bin);
        const char *b = bin.UTF8String;
        int sum = 0;
        parsePacket(b, 0, &sum);
        printf("%d\n", sum);
    }
    return 0;
}
