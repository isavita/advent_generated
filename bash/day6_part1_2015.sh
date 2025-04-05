
#!/bin/bash

main() {
    awk '
    BEGIN { FS = "[ ,]" } # Set field separators to space or comma

    /turn on/ {
        x1=$3; y1=$4; x2=$6; y2=$7
        for (i=x1; i<=x2; i++) {
            for (j=y1; j<=y2; j++) {
                lights[i","j] = 1
            }
        }
    }

    /turn off/ {
        x1=$3; y1=$4; x2=$6; y2=$7
        for (i=x1; i<=x2; i++) {
            for (j=y1; j<=y2; j++) {
                lights[i","j] = 0
            }
        }
    }

    /toggle/ {
        x1=$2; y1=$3; x2=$5; y2=$6
        for (i=x1; i<=x2; i++) {
            for (j=y1; j<=y2; j++) {
                key = i","j
                lights[key] = 1 - lights[key]
            }
        }
    }

    END {
        count = 0
        for (key in lights) {
            if (lights[key] == 1) {
                count++
            }
        }
        print count
    }
    ' input.txt
}

main
