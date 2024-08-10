#import <Foundation/Foundation.h>

typedef struct {
    NSInteger pos, val;
} num;

void mix(num *nums, NSInteger count);
NSInteger coords(num *nums, NSInteger count);
NSString* readAll(NSString *path);
NSInteger toInt(NSString *s);

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = readAll(@"input.txt");
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        NSInteger count = lines.count;
        num nums[count];
        
        for (NSInteger i = 0; i < count; i++) {
            nums[i].pos = i;
            nums[i].val = toInt(lines[i]);
        }

        num nums2[count];
        for (NSInteger i = 0; i < count; i++) {
            nums2[i] = (num){nums[i].pos, 811589153 * nums[i].val};
        }

        for (NSInteger i = 0; i < 10; i++) {
            mix(nums2, count);
        }
        
        NSLog(@"%ld", (long)coords(nums2, count));
    }
    return 0;
}

void mix(num *nums, NSInteger count) {
    NSInteger n = count - 1;
    for (NSInteger i = 0; i < count; i++) {
        NSInteger oldpos = nums[i].pos;
        NSInteger newpos = ((oldpos + nums[i].val) % n + n) % n;
        if (oldpos < newpos) {
            for (NSInteger j = 0; j < count; j++) {
                if (nums[j].pos > oldpos && nums[j].pos <= newpos) {
                    nums[j].pos--;
                }
            }
        }
        if (newpos < oldpos) {
            for (NSInteger j = 0; j < count; j++) {
                if (nums[j].pos >= newpos && nums[j].pos < oldpos) {
                    nums[j].pos++;
                }
            }
        }
        nums[i].pos = newpos;
    }
}

NSInteger coords(num *nums, NSInteger count) {
    NSInteger zeroPos = 0;
    for (NSInteger i = 0; i < count; i++) {
        if (nums[i].val == 0) {
            zeroPos = nums[i].pos;
            break;
        }
    }
    NSInteger sum = 0;
    for (NSInteger i = 0; i < count; i++) {
        if (nums[i].pos == (zeroPos + 1000) % count || 
            nums[i].pos == (zeroPos + 2000) % count || 
            nums[i].pos == (zeroPos + 3000) % count) {
            sum += nums[i].val;
        }
    }
    return sum;
}

NSString* readAll(NSString *path) {
    NSError *error = nil;
    NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        @throw [NSException exceptionWithName:@"FileReadException" reason:error.localizedDescription userInfo:nil];
    }
    return [content stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
}

NSInteger toInt(NSString *s) {
    return [s integerValue];
}