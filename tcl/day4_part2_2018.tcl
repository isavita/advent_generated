
set fp [open "input.txt" r]
set lines [lsort [split [read $fp] \n]]
close $fp

array set guards {}
set cur -1

foreach line $lines {
    if {$line eq ""} continue
    set min [scan [string range $line 15 16] %d]
    
    if {[regexp {#(\d+)} $line -> id]} {
        set cur $id
        if {![info exists guards($cur)]} {
            set guards($cur) [lrepeat 60 0]
        }
    } elseif {[string match "*asleep*" $line]} {
        set start $min
    } elseif {[string match "*wakes*" $line]} {
        set mins $guards($cur)
        for {set i $start} {$i < $min} {incr i} {
            lset mins $i [expr {[lindex $mins $i] + 1}]
        }
        set guards($cur) $mins
    }
}

# Part 1
set max_sleep -1
set sleepiest ""
foreach g [array names guards] {
    set total [tcl::mathop::+ {*}$guards($g)]
    if {$total > $max_sleep} {
        set max_sleep $total
        set sleepiest $g
    }
}

set max_mins -1
set best_min -1
set m 0
foreach c $guards($sleepiest) {
    if {$c > $max_mins} {
        set max_mins $c
        set best_min $m
    }
    incr m
}
puts [expr {$sleepiest * $best_min}]

# Part 2
set glob_max -1
set glob_id -1
set glob_min -1

foreach g [array names guards] {
    set m 0
    foreach c $guards($g) {
        if {$c > $glob_max} {
            set glob_max $c
            set glob_id $g
            set glob_min $m
        }
        incr m
    }
}
puts [expr {$glob_id * $glob_min}]
