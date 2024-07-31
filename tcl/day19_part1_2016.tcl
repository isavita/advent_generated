proc readInput {filename} {
    set file [open $filename r]
    set totalElves [gets $file]
    close $file
    return $totalElves
}

proc findWinningElf {totalElves} {
    set highestPowerOfTwo 1
    while {$highestPowerOfTwo * 2 <= $totalElves} {
        set highestPowerOfTwo [expr {$highestPowerOfTwo * 2}]
    }
    return [expr {($totalElves - $highestPowerOfTwo) * 2 + 1}]
}

set totalElves [readInput "input.txt"]
set winner [findWinningElf $totalElves]
puts $winner