
#import <Foundation/Foundation.h>

typedef struct {
    long long pos;
    long long val;
} Num;

static void mix(Num *nums, NSUInteger size) {
    long long n = size - 1;
    for (NSUInteger i = 0; i < size; ++i) {
        long long oldPos = nums[i].pos;
        long long newPos = ((oldPos + nums[i].val) % n + n) % n;
        if (oldPos < newPos) {
            for (NSUInteger j = 0; j < size; ++j) {
                if (nums[j].pos > oldPos && nums[j].pos <= newPos) {
                    nums[j].pos--;
                }
            }
        } else if (newPos < oldPos) {
            for (NSUInteger j = 0; j < size; ++j) {
                if (nums[j].pos >= newPos && nums[j].pos < oldPos) {
                    nums[j].pos++;
                }
            }
        }
        nums[i].pos = newPos;
    }
}

static long long coords(Num *nums, NSUInteger size) {
    long long zeroPos = 0;
    for (NSUInteger i = 0; i < size; ++i) {
        if (nums[i].val == 0) {
            zeroPos = nums[i].pos;
            break;
        }
    }
    long long sum = 0;
    for (NSUInteger i = 0; i < size; ++i) {
        long long p = nums[i].pos;
        if (p == (zeroPos + 1000) % size ||
            p == (zeroPos + 2000) % size ||
            p == (zeroPos + 3000) % size) {
            sum += nums[i].val;
        }
    }
    return sum;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *err = nil;
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
        if (!content) {
            fprintf(stderr, "Error opening file: %s\n", err.localizedDescription.UTF8String);
            return -1;
        }

        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSUInteger size = 0;
        Num *nums = NULL;

        for (NSString *line in lines) {
            if (line.length == 0) continue;
            long long v = line.longLongValue;
            Num num = { .pos = size, .val = v };
            ++size;
            nums = realloc(nums, size * sizeof(Num));
            nums[size - 1] = num;
        }

        mix(nums, size);
        long long result = coords(nums, size);
        printf("%lld\n", result);

        free(nums);
    }
    return 0;
}
