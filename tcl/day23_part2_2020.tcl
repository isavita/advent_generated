proc readInput {filename} {
    set file [open $filename r]
    set line [gets $file]
    close $file
    return [split $line ""]
}

proc playGame {cups moves} {
    array set nextCup {}
    set maxCup [llength $cups]

    for {set i 0} {$i < [llength $cups] - 1} {incr i} {
        set nextCup([lindex $cups $i]) [lindex $cups [expr {$i + 1}]]
    }
    set nextCup([lindex $cups end]) [lindex $cups 0]

    set currentCup [lindex $cups 0]
    for {set i 0} {$i < $moves} {incr i} {
        set pickup [list]
        set pickupCup $currentCup
        for {set j 0} {$j < 3} {incr j} {
            set pickupCup $nextCup($pickupCup)
            lappend pickup $pickupCup
        }
        set nextCup($currentCup) $nextCup([lindex $pickup 2])

        set destCup [expr {$currentCup - 1}]
        if {$destCup < 1} {set destCup $maxCup}
        while {[lsearch -exact $pickup $destCup] != -1} {
            set destCup [expr {$destCup - 1}]
            if {$destCup < 1} {set destCup $maxCup}
        }

        set nextCup([lindex $pickup 2]) $nextCup($destCup)
        set nextCup($destCup) [lindex $pickup 0]
        set currentCup $nextCup($currentCup)
    }
    return [array get nextCup]
}

proc part1 {cups} {
    set nextCup [playGame $cups 100]
    set result ""
    set cup 1
    for {set i 0} {$i < [llength $cups] - 1} {incr i} {
        set cup [dict get $nextCup $cup]
        append result $cup
    }
    return $result
}

proc part2 {cups} {
    set maxCup [llength $cups]
    for {set i [expr {$maxCup + 1}]} {$i <= 1000000} {incr i} {
        lappend cups $i
    }
    set nextCup [playGame $cups 10000000]
    set firstCup [dict get $nextCup 1]
    set secondCup [dict get $nextCup $firstCup]
    return [expr {$firstCup * $secondCup}]
}

set input [readInput "input.txt"]
set cups [list]
foreach cup $input {
    lappend cups [expr {$cup}]
}

puts "Part 1: [part1 $cups]"
puts "Part 2: [part2 $cups]"