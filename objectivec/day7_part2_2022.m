#import <Foundation/Foundation.h>

@interface Directory : NSObject
@property (nonatomic, strong) NSMutableDictionary<NSString*, Directory*> *subdirectories;
@property (nonatomic, strong) NSMutableDictionary<NSString*, NSNumber*> *files;
@property (nonatomic, strong) Directory *parent;
@property (nonatomic, assign) int64_t totalSize;
@end

@implementation Directory
- (instancetype)init {
    self = [super init];
    if (self) {
        _subdirectories = [NSMutableDictionary dictionary];
        _files = [NSMutableDictionary dictionary];
        _totalSize = 0;
    }
    return self;
}
@end

void parseCommands(NSArray<NSString*> *commands, Directory *root) {
    Directory *currentDirectory = root;
    for (NSString *command in commands) {
        if ([command hasPrefix:@"$ cd"]) {
            NSString *dirName = [[command componentsSeparatedByString:@" "] lastObject];
            if ([dirName isEqualToString:@"/"]) {
                currentDirectory = root;
            } else if ([dirName isEqualToString:@".."]) {
                currentDirectory = currentDirectory.parent;
            } else {
                currentDirectory = currentDirectory.subdirectories[dirName];
            }
        } else if ([command hasPrefix:@"$ ls"]) {
            // List command does nothing in this context
        } else {
            NSArray *components = [command componentsSeparatedByString:@" "];
            if ([components[0] isEqualToString:@"dir"]) {
                NSString *dirName = components[1];
                Directory *newDir = [[Directory alloc] init];
                newDir.parent = currentDirectory;
                currentDirectory.subdirectories[dirName] = newDir;
            } else {
                int64_t fileSize = [components[0] longLongValue];
                NSString *fileName = components[1];
                currentDirectory.files[fileName] = @(fileSize);
                Directory *dir = currentDirectory;
                while (dir != nil) {
                    dir.totalSize += fileSize;
                    dir = dir.parent;
                }
            }
        }
    }
}

void calculateDirectorySizes(Directory *directory, NSMutableArray<NSNumber*> *sizes) {
    for (Directory *subdir in directory.subdirectories.allValues) {
        calculateDirectorySizes(subdir, sizes);
    }
    [sizes addObject:@(directory.totalSize)];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString*> *commands = [fileContents componentsSeparatedByString:@"\n"];

        Directory *root = [[Directory alloc] init];
        parseCommands(commands, root);

        NSMutableArray<NSNumber*> *sizes = [NSMutableArray array];
        calculateDirectorySizes(root, sizes);

        int64_t sumOfSizes = 0;
        for (NSNumber *size in sizes) {
            if (size.longLongValue <= 100000) {
                sumOfSizes += size.longLongValue;
            }
        }
        printf("Sum of total sizes of directories with size at most 100000: %lld\n", sumOfSizes);

        int64_t totalDiskSpace = 70000000;
        int64_t requiredSpace = 30000000;
        int64_t usedSpace = root.totalSize;
        int64_t freeSpace = totalDiskSpace - usedSpace;
        int64_t neededSpace = requiredSpace - freeSpace;

        int64_t smallestDirectorySize = INT64_MAX;
        for (NSNumber *size in sizes) {
            if (size.longLongValue >= neededSpace && size.longLongValue < smallestDirectorySize) {
                smallestDirectorySize = size.longLongValue;
            }
        }
        printf("Total size of the smallest directory to delete: %lld\n", smallestDirectorySize);
    }
    return 0;
}