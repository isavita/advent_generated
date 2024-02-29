#!/bin/bash

horizontalPosition=0
depth=0
aim=0

while IFS=' ' read -r direction units || [[ -n "$direction" ]]; do
    case $direction in
        forward)
            let horizontalPosition+=units
            let depth+=aim*units
            ;;
        down)
            let aim+=units
            ;;
        up)
            let aim-=units
            ;;
    esac
done < "input.txt"

let product=horizontalPosition*depth
echo $product