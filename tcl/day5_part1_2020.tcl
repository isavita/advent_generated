set file [open "input.txt" r]
set maxSeatID 0

proc binaryToInt {binaryStr} {
    set result 0
    set len [string length $binaryStr]
    for {set i 0} {$i < $len} {incr i} {
        if {[string index $binaryStr $i] == "1"} {
            set result [expr {$result + 2 ** ($len - $i - 1)}]
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
    set pass [string map {"F" "0" "B" "1" "L" "0" "R" "1"} $line]
    set seatID [decode $pass]
    if {$seatID > $maxSeatID} {
        set maxSeatID $seatID
    }
}

close $file
puts $maxSeatID