
# Tcl script to compute the sum of special IDs from ranges in input.txt

set f [open "input.txt"]
set data [read $f]
close $f

regsub -all {[ \r\n]+} $data , data
set tokens [split $data ,]

set ids [dict create]

foreach token $tokens {
    if {$token eq ""} continue
    if {[string first - $token] == -1} continue
    set parts [split $token -]
    set start [lindex $parts 0]
    set end   [lindex $parts 1]
    if {$start > $end} {
        set tmp $start
        set start $end
        set end $tmp
    }
    for {set k 1} {$k <= 10} {incr k} {
        set pow10k [expr {10**$k}]
        set multiplier [expr {$pow10k + 1}]
        set minSeed [expr {10**($k-1)}]
        set maxSeed [expr {$pow10k - 1}]
        set sMin [expr {($start + $multiplier - 1) / $multiplier}]
        set sMax [expr {$end / $multiplier}]
        if {$sMin < $minSeed} {set sMin $minSeed}
        if {$sMax > $maxSeed} {set sMax $maxSeed}
        if {$sMin <= $sMax} {
            for {set seed $sMin} {$seed <= $sMax} {incr seed} {
                set id [expr {$seed * $multiplier}]
                dict set ids $id 1
            }
        }
    }
}

set sum 0
foreach id [dict keys $ids] {
    set sum [expr {$sum + $id}]
}
puts $sum
