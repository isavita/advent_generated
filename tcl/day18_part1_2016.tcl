set totalRows 40

# Open the file and read the contents
set fileId [open "input.txt" r]
set firstRow [read $fileId]
close $fileId

proc countSafeTiles {firstRow totalRows} {
    set currentRow $firstRow
    set safeCount [countChar $currentRow .]

    for {set i 1} {$i < $totalRows} {incr i} {
        set nextRow ""
        set len [string length $currentRow]
        for {set j 0} {$j < $len} {incr j} {
            set left [safeIfOutOfBounds [expr {$j - 1}] $currentRow]
            set center [string index $currentRow $j]
            set right [safeIfOutOfBounds [expr {$j + 1}] $currentRow]
            if {[isTrap $left $center $right]} {
                append nextRow "^"
            } else {
                append nextRow "."
                incr safeCount
            }
        }
        set currentRow $nextRow
    }
    return $safeCount
}

proc isTrap {left center right} {
    return [expr {($left == "^" && $center == "^" && $right == ".") ||
                  ($center == "^" && $right == "^" && $left == ".") ||
                  ($left == "^" && $center == "." && $right == ".") ||
                  ($right == "^" && $center == "." && $left == ".")}]
}

proc safeIfOutOfBounds {index row} {
    if {$index < 0 || $index >= [string length $row]} {
        return "."
    }
    return [string index $row $index]
}

proc countChar {str char} {
    set count 0
    foreach c [split $str ""] {
        if {$c == $char} {
            incr count
        }
    }
    return $count
}

set safeTilesCount [countSafeTiles $firstRow $totalRows]
puts $safeTilesCount