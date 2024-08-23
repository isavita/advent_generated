proc hexToBinary {hex} {
    set binary ""
    foreach c [split $hex ""] {
        switch $c {
            0 {append binary "0000"}
            1 {append binary "0001"}
            2 {append binary "0010"}
            3 {append binary "0011"}
            4 {append binary "0100"}
            5 {append binary "0101"}
            6 {append binary "0110"}
            7 {append binary "0111"}
            8 {append binary "1000"}
            9 {append binary "1001"}
            A {append binary "1010"}
            B {append binary "1011"}
            C {append binary "1100"}
            D {append binary "1101"}
            E {append binary "1110"}
            F {append binary "1111"}
        }
    }
    return $binary
}

proc parsePacket {binary index} {
    set version [binaryToInt [string range $binary $index [expr {$index + 2}]]]
    set typeID [binaryToInt [string range $binary [expr {$index + 3}] [expr {$index + 5}]]]
    set index [expr {$index + 6}]
    set versionSum $version

    if {$typeID == 4} {
        while {[string index $binary $index] == "1"} {
            set index [expr {$index + 5}]
        }
        set index [expr {$index + 5}]
    } else {
        set lengthTypeID [string index $binary $index]
        set index [expr {$index + 1}]
        if {$lengthTypeID == "0"} {
            set totalLength [binaryToInt [string range $binary $index [expr {$index + 14}]]]
            set index [expr {$index + 15}]
            set end [expr {$index + $totalLength}]
            while {$index < $end} {
                set result [parsePacket $binary $index]
                set versionSum [expr {$versionSum + [lindex $result 0]}]
                set index [lindex $result 1]
            }
        } else {
            set numSubPackets [binaryToInt [string range $binary $index [expr {$index + 10}]]]
            set index [expr {$index + 11}]
            for {set i 0} {$i < $numSubPackets} {incr i} {
                set result [parsePacket $binary $index]
                set versionSum [expr {$versionSum + [lindex $result 0]}]
                set index [lindex $result 1]
            }
        }
    }
    return [list $versionSum $index]
}

proc binaryToInt {binary} {
    set value 0
    foreach bit [split $binary ""] {
        set value [expr {$value * 2 + $bit}]
    }
    return $value
}

set file [open "input.txt" r]
set hex [read $file]
close $file

set binary [hexToBinary $hex]
set result [parsePacket $binary 0]
puts [lindex $result 0]