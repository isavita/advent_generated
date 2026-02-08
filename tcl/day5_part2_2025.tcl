
set f [open input.txt r]
set ranges {}
while {[gets $f line] >= 0} {
    if {[regexp {^\s*(\d+)-(\d+)\s*$} $line -> a b]} {
        if {$a > $b} {lassign [list $b $a] a b}
        lappend ranges [list $a $b]
    }
}
close $f

if {[llength $ranges] == 0} {
    puts "Total fresh IDs: 0"
    exit
}

set ranges [lsort -index 0 -integer $ranges]

set total 0
lassign [lindex $ranges 0] curMin curMax
foreach range [lrange $ranges 1 end] {
    lassign $range min max
    if {$min <= $curMax} {
        if {$max > $curMax} {set curMax $max}
    } else {
        incr total [expr {$curMax - $curMin + 1}]
        set curMin $min
        set curMax $max
    }
}
incr total [expr {$curMax - $curMin + 1}]

puts "Total fresh IDs: $total"
