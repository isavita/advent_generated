#!/bin/bash

awk -F' -> |,' '{
    x1=$1; y1=$2; x2=$3; y2=$4;
    if (x1 == x2) {
        if (y1 > y2) { tmp=y1; y1=y2; y2=tmp; }
        for (y=y1; y<=y2; y++) grid[x1","y]++;
    } else if (y1 == y2) {
        if (x1 > x2) { tmp=x1; x1=x2; x2=tmp; }
        for (x=x1; x<=x2; x++) grid[x","y1]++;
    }
} END {
    overlapCount=0;
    for (i in grid) {
        if (grid[i] > 1) overlapCount++;
    }
    print overlapCount;
}' input.txt