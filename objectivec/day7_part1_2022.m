#import <Foundation/Foundation.h>

@interface Directory : NSObject
@property (nonatomic, strong) NSMutableDictionary<NSString*, Directory*> *subdirectories;
@property (nonatomic, strong) NSMutableArray<NSNumber*> *files;
@property (nonatomic, assign) NSInteger totalSize;

- (instancetype)init;
- (void)calculateTotalSize;
@end

@implementation Directory

- (instancetype)init {
    self = [super init];
    if (self) {
        _subdirectories = [NSMutableDictionary dictionary];
        _files = [NSMutableArray array];
        _totalSize = 0;
    }
    return self;
}

- (void)calculateTotalSize {
    self.totalSize = 0;
    for (NSNumber *fileSize in self.files) {
        self.totalSize += fileSize.integerValue;
    }
    for (Directory *subdir in self.subdirectories.allValues) {
        [subdir calculateTotalSize];
        self.totalSize += subdir.totalSize;
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];

        Directory *root = [[Directory alloc] init];
        Directory *currentDirectory = root;
        NSMutableArray *path = [NSMutableArray arrayWithObject:root];

        for (NSString *line in lines) {
            if ([line hasPrefix:@"$ cd"]) {
                NSString *dirName = [[line componentsSeparatedByString:@" "] lastObject];
                if ([dirName isEqualToString:@"/"]) {
                    currentDirectory = root;
                    [path removeAllObjects];
                    [path addObject:root];
                } else if ([dirName isEqualToString:@".."]) {
                    [path removeLastObject];
                    currentDirectory = [path lastObject];
                } else {
                    currentDirectory = currentDirectory.subdirectories[dirName];
                    [path addObject:currentDirectory];
                }
            } else if ([line hasPrefix:@"$ ls"]) {
                // Do nothing, just listing
            } else {
                NSArray *parts = [line componentsSeparatedByString:@" "];
                if ([parts[0] isEqualToString:@"dir"]) {
                    NSString *dirName = parts[1];
                    if (!currentDirectory.subdirectories[dirName]) {
                        currentDirectory.subdirectories[dirName] = [[Directory alloc] init];
                    }
                } else {
                    NSInteger fileSize = [parts[0] integerValue];
                    [currentDirectory.files addObject:@(fileSize)];
                }
            }
        }

        [root calculateTotalSize];

        NSInteger sumOfSizes = 0;
        NSMutableArray *directoriesToCheck = [NSMutableArray arrayWithObject:root];
        while (directoriesToCheck.count > 0) {
            Directory *directory = [directoriesToCheck lastObject];
            [directoriesToCheck removeLastObject];
            if (directory.totalSize <= 100000) {
                sumOfSizes += directory.totalSize;
            }
            [directoriesToCheck addObjectsFromArray:directory.subdirectories.allValues];
        }

        NSLog(@"Sum of total sizes of directories with a total size of at most 100000: %ld", (long)sumOfSizes);
    }
    return 0;
}