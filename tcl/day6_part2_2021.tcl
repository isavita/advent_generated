
proc solve {filename} {
    set file [open $filename r]
    set line [gets $file]
    close $file
    set fishAges [split $line ,]
    
    array set lanternFishCounts [list]
    for {set i 0} {$i < 9} {incr i} {
        set lanternFishCounts($i) 0
    }

    foreach age $fishAges {
        incr lanternFishCounts($age)
    }

    for {set i 0} {$i < 256} {incr i} {
        set newLanternFish $lanternFishCounts(0)
        for {set j 0} {$j < 8} {incr j} {
            set lanternFishCounts($j) $lanternFishCounts([expr {$j+1}])
        }
        incr lanternFishCounts(6) $newLanternFish
        set lanternFishCounts(8) $newLanternFish
    }

    set sum 0
    for {set i 0} {$i < 9} {incr i} {
        incr sum $lanternFishCounts($i)
    }
    puts $sum
}

solve "input.txt"
