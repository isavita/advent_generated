
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        if (!input) return 1;
        input = [input stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSUInteger len = input.length;
        if (len == 0) { printf("0\n"); return 0; }

        NSUInteger total = 0;
        for (NSUInteger i = 0; i < len; i++)
            total += [input characterAtIndex:i] - '0';

        int *disk = calloc(total, sizeof(int));
        NSMutableArray *files = [NSMutableArray array];

        NSUInteger pos = 0;
        int fileID = 0;
        for (NSUInteger i = 0; i < len; i++) {
            NSUInteger run = [input characterAtIndex:i] - '0';
            if (i % 2 == 0) {
                [files addObject:@{@0:@(fileID), @1:@(pos), @2:@(pos+run-1)}];
                for (NSUInteger j = 0; j < run; j++) disk[pos+j] = fileID;
                fileID++;
            } else {
                for (NSUInteger j = 0; j < run; j++) disk[pos+j] = -1;
            }
            pos += run;
        }

        for (NSInteger k = files.count - 1; k >= 0; k--) {
            NSDictionary *f = files[k];
            int id = [f[@0] intValue];
            int start = [f[@1] intValue];
            int end = [f[@2] intValue];
            int flen = end - start + 1;

            int best = -1, span = 0;
            for (int j = 0; j < start; j++) {
                if (disk[j] == -1) {
                    if (span == 0) best = j;
                    if (++span == flen) break;
                } else {
                    span = 0;
                }
            }
            if (span == flen) {
                for (int j = 0; j < flen; j++) disk[best+j] = id;
                for (int j = start; j <= end; j++) disk[j] = -1;
            }
        }

        long long checksum = 0;
        for (NSUInteger i = 0; i < total; i++)
            if (disk[i] != -1) checksum += i * disk[i];

        printf("%lld\n", checksum);
        free(disk);
    }
    return 0;
}
