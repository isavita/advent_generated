proc readInput {filename} {
    set file [open $filename r]
    set numElves [gets $file]
    close $file
    return $numElves
}

proc findWinningElf {numElves} {
    set highestPowerOfThree 1
    while {$highestPowerOfThree * 3 < $numElves} {
        set highestPowerOfThree [expr {$highestPowerOfThree * 3}]
    }
    return [expr {$numElves - $highestPowerOfThree}]
}

set numElves [readInput "input.txt"]
set winningElf [findWinningElf $numElves]
puts $winningElf