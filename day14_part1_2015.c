
#include <stdio.h>
#include <stdlib.h>

#define MAX_REINDEER 10

typedef struct {
    char name[20];
    int speed;
    int fly_time;
    int rest_time;
} Reindeer;

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    
    Reindeer reindeers[MAX_REINDEER];
    int i = 0;
    while (fscanf(fp, "%s can fly %d km/s for %d seconds, but then must rest for %d seconds.", reindeers[i].name, &reindeers[i].speed, &reindeers[i].fly_time, &reindeers[i].rest_time) != EOF) {
        i++;
    }
    
    int max_distance = 0;
    for (int j = 0; j < MAX_REINDEER; j++) {
        int distance = 0;
        int time = 2503;
        while (time > 0) {
            if (time >= reindeers[j].fly_time) {
                distance += reindeers[j].speed * reindeers[j].fly_time;
                time -= reindeers[j].fly_time;
            } else {
                distance += reindeers[j].speed * time;
                time = 0;
            }
            time -= reindeers[j].rest_time;
        }
        if (distance > max_distance) {
            max_distance = distance;
        }
    }
    
    printf("%d\n", max_distance);
    
    fclose(fp);
    
    return 0;
}
