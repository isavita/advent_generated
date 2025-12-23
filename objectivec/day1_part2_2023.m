
#import <Foundation/Foundation.h>

static const char *digitWords[10] = {"zero","one","two","three","four","five","six","seven","eight","nine"};
static const size_t digitLengths[10] = {4,3,3,5,4,4,3,5,5,4};

int findFirstAndLastDigit(const char *line) {
    int first = -1, last = -1;
    for (size_t i = 0; line[i]; ++i) {
        char c = line[i];
        if (c >= '0' && c <= '9') {
            int d = c - '0';
            if (first == -1) first = d;
            last = d;
            continue;
        }
        for (int d = 0; d < 10; ++d) {
            if (strncmp(&line[i], digitWords[d], digitLengths[d]) == 0) {
                if (first == -1) first = d;
                last = d;
                break;
            }
        }
    }
    return first * 10 + last;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 1;
        __block long long sum = 0;
        [content enumerateLinesUsingBlock:^(NSString *line, BOOL *stop) {
            sum += findFirstAndLastDigit(line.UTF8String);
        }];
        printf("%lld\n", sum);
    }
    return 0;
}
