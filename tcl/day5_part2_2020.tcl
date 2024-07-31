set file [open "input.txt" r]
set seatIDs {}

proc binaryToInt {binaryStr} {
    set result 0
    set len [string length $binaryStr]
    for {set i 0} {$i < $len} {incr i} {
        if {[string index $binaryStr $i] == "1"} {
            set result [expr {$result + (1 << ($len - $i - 1))}]
        }
    }
    return $result
}

proc decode {pass} {
    set row [binaryToInt [string range $pass 0 6]]
    set column [binaryToInt [string range $pass 7 9]]
    return [expr {$row * 8 + $column}]
}

while {[gets $file line] >= 0} {
    set pass [string map {F 0 B 1 L 0 R 1} $line]
    lappend seatIDs [decode $pass]
}

close $file
set seatIDs [lsort -integer $seatIDs]

for {set i 0} {$i < [llength $seatIDs] - 1} {incr i} {
    if {[lindex $seatIDs [expr {$i + 1}]] != [lindex $seatIDs $i] + 1} {
        puts [expr {[lindex $seatIDs $i] + 1}]
        break
    }
}