proc readInput {filename} {
    set file [open $filename r]
    set data [read $file]
    close $file
    return [split $data "\n"]
}

proc snafuToDecimal {snafu} {
    set decimal 0
    set power 1
    set length [string length $snafu]
    for {set i [expr {$length - 1}]} {$i >= 0} {incr i -1} {
        set digit [string index $snafu $i]
        switch $digit {
            "2" {set value 2}
            "1" {set value 1}
            "0" {set value 0}
            "-" {set value -1}
            "=" {set value -2}
        }
        set decimal [expr {$decimal + $value * $power}]
        set power [expr {$power * 5}]
    }
    return $decimal
}

proc decimalToSnafu {decimal} {
    set result ""
    while {$decimal != 0} {
        set remainder [expr {$decimal % 5}]
        set decimal [expr {$decimal / 5}]
        switch $remainder {
            0 {set result "0$result"}
            1 {set result "1$result"}
            2 {set result "2$result"}
            3 {
                set result "=$result"
                set decimal [expr {$decimal + 1}]
            }
            4 {
                set result "-$result"
                set decimal [expr {$decimal + 1}]
            }
        }
    }
    return $result
}

set lines [readInput "input.txt"]
set total 0
foreach line $lines {
    set total [expr {$total + [snafuToDecimal $line]}]
}
puts [decimalToSnafu $total]