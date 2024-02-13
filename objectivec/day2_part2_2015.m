#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *file = fopen("input.txt", "r");
        if (file == NULL) {
            printf("Error opening the file\n");
            return 1;
        }
        
        int l, w, h;
        int totalPaper = 0;
        int totalRibbon = 0;
        
        while (fscanf(file, "%dx%dx%d\n", &l, &w, &h) != EOF) {
            int side1 = l * w;
            int side2 = w * h;
            int side3 = h * l;
            int slack = MIN(MIN(side1, side2), side3);
            
            totalPaper += 2*l*w + 2*w*h + 2*h*l + slack;
            totalRibbon += 2*l + 2*w + 2*h - 2*MAX(MAX(l, w), h) + l*w*h;
        }
        
        printf("Total square feet of wrapping paper: %d\n", totalPaper);
        printf("Total feet of ribbon: %d\n", totalRibbon);
        
        fclose(file);
    }
    return 0;
}