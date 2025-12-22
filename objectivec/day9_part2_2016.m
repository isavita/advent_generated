
#import <Foundation/Foundation.h>

long long decompress(const char *s, int start, int end) {
    long long len = 0;
    for (int i = start; i < end; ) {
        if (s[i] == '(') {
            int chars = 0, reps = 0, j = i + 1;
            while (isdigit(s[j])) chars = chars * 10 + (s[j++] - '0');
            j++; // skip 'x'
            while (isdigit(s[j])) reps = reps * 10 + (s[j++] - '0');
            int next = j + 1;
            len += reps * decompress(s, next, next + chars);
            i = next + chars;
        } else {
            len++;
            i++;
        }
    }
    return len;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSData *data = [NSData dataWithContentsOfFile:@"input.txt"];
        if (!data) return 1;
        NSString *str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        const char *cstr = [str UTF8String];
        long long result = decompress(cstr, 0, (int)strlen(cstr));
        printf("%lld\n", result);
    }
    return 0;
}
