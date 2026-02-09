
set fp [open "input.txt" r]
set data [read $fp]
close $fp

set state {}
set rules [dict create]

foreach line [split $data "\n"] {
    if {[string match "initial state:*" $line]} {
        set init [string trim [string range $line 15 end]]
        for {set i 0} {$i < [string length $init]} {incr i} {
            if {[string index $init $i] eq "#"} {
                lappend state $i
            }
        }
    } elseif {[regexp {([.#]{5}) => ([.#])} $line -> pattern result]} {
        dict set rules $pattern $result
    }
}

set prevPattern ""
set prevSum 0
set generations 50000000000

for {set gen 0} {$gen < $generations} {incr gen} {
    set map [dict create]
    foreach p $state { dict set map $p 1 }
    
    set newState {}
    set min [lindex $state 0]
    set max [lindex $state end]
    
    for {set i [expr {$min - 2}]} {$i <= [expr {$max + 2}]} {incr i} {
        set pattern ""
        for {set j -2} {$j <= 2} {incr j} {
            append pattern [expr {[dict exists $map [expr {$i + $j}]] ? "#" : "."}]
        }
        if {[dict exists $rules $pattern] && [dict get $rules $pattern] eq "#"} {
            lappend newState $i
        }
    }
    
    set state $newState
    set sum [tcl::mathop::+ {*}$state]
    
    set map2 [dict create]
    foreach p $state { dict set map2 $p 1 }
    set min2 [lindex $state 0]
    set max2 [lindex $state end]
    
    set currPattern ""
    for {set i $min2} {$i <= $max2} {incr i} {
        append currPattern [expr {[dict exists $map2 $i] ? "#" : "."}]
    }
    
    if {$currPattern eq $prevPattern} {
        set offset [expr {$sum - $prevSum}]
        set remaining [expr {$generations - $gen - 1}]
        puts [expr {$sum + $offset * $remaining}]
        exit
    }
    
    set prevPattern $currPattern
    set prevSum $sum
}

puts $sum
