set file [open "input.txt" r]
set firstRow [gets $file]
close $file

proc countSafeTiles {firstRow totalRows} {
    set currentRow $firstRow
    set safeCount 0

    for {set i 0} {$i < $totalRows} {incr i} {
        foreach tile [split $currentRow ""] {
            if {$tile eq "."} {
                incr safeCount
            }
        }
        set nextRow ""
        set length [string length $currentRow]
        for {set j 0} {$j < $length} {incr j} {
            set left [expr {$j > 0 ? [string index $currentRow [expr {$j - 1}]] : "."}]
            set center [string index $currentRow $j]
            set right [expr {$j < $length - 1 ? [string index $currentRow [expr {$j + 1}]] : "."}]
            if {($left eq "^" && $center eq "^" && $right eq ".") ||
                ($center eq "^" && $right eq "^" && $left eq ".") ||
                ($left eq "^" && $center eq "." && $right eq ".") ||
                ($right eq "^" && $center eq "." && $left eq ".")} {
                append nextRow "^"
            } else {
                append nextRow "."
            }
        }
        set currentRow $nextRow
    }
    return $safeCount
}

set totalSafeTiles [countSafeTiles $firstRow 400000]
puts $totalSafeTiles