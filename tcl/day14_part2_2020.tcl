proc generateAddresses {mask address} {
    set floating {}
    for {set i 0} {$i < [string length $mask]} {incr i} {
        set bit [string index $mask $i]
        if {$bit eq "1"} {
            set address [expr {$address | (1 << (35 - $i))}]
        } elseif {$bit eq "X"} {
            lappend floating [expr {35 - $i}]
        }
    }
    set addresses {}
    set count [expr {1 << [llength $floating]}]
    for {set i 0} {$i < $count} {incr i} {
        set modAddress $address
        for {set j 0} {$j < [llength $floating]} {incr j} {
            set pos [lindex $floating $j]
            if {[expr {$i & (1 << $j)}] == 0} {
                set modAddress [expr {$modAddress & ~(1 << $pos)}]
            } else {
                set modAddress [expr {$modAddress | (1 << $pos)}]
            }
        }
        lappend addresses $modAddress
    }
    return $addresses
}

set file [open "input.txt" r]
set mask ""
set mem [dict create]

while {[gets $file line] >= 0} {
    if {[string match "mask = *" $line]} {
        set mask [string trim [string range $line 7 end]]
    } else {
        regexp {mem\[(\d+)\] = (\d+)} $line -> address value
        set addresses [generateAddresses $mask $address]
        foreach addr $addresses {
            dict set mem $addr $value
        }
    }
}
close $file

set sum 0
foreach value [dict values $mem] {
    set sum [expr {$sum + $value}]
}
puts $sum