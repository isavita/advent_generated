set file [open "input.txt" r]
set mask ""
array set mem {}

proc applyMask {value mask} {
    set result 0
    for {set i 0} {$i < 36} {incr i} {
        set bitValue 0
        set bitValue [expr {1 << (35 - $i)}]
        if {[string index $mask $i] eq "1"} {
            set result [expr {$result | $bitValue}]
        } elseif {[string index $mask $i] eq "X"} {
            set result [expr {$result | ($value & $bitValue)}]
        }
    }
    return $result
}

while {[gets $file line] >= 0} {
    if {[string match "mask = *" $line]} {
        set mask [string trim [string range $line 7 end]]
    } elseif {[regexp {mem\[(\d+)] = (\d+)} $line match address value]} {
        set address [expr {$address}]
        set value [expr {$value}]
        set mem($address) [applyMask $value $mask]
    }
}

set sum 0
foreach value [array names mem] {
    set sum [expr {$sum + $mem($value)}]
}

puts $sum
close $file