
#import <Foundation/Foundation.h>

int compareInts(const void *a, const void *b) {
    return *(int *)a - *(int *)b;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *error = nil;
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&error];
        if (!content) return 1;
        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        int left[10000], right[10000];
        int count = 0;
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            int l, r;
            if (sscanf([line UTF8String], "%d %d", &l, &r) == 2) {
                left[count] = l;
                right[count] = r;
                count++;
            }
        }
        qsort(left, count, sizeof(int), compareInts);
        qsort(right, count, sizeof(int), compareInts);
        long total = 0;
        for (int i = 0; i < count; i++) total += labs((long)left[i] - (long)right[i]);
        printf("%ld\n", total);
    }
    return 0;
}
