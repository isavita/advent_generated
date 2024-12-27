
proc hexToBin {hex} {
    set bin ""
    foreach char [split $hex ""] {
        switch -exact -- $char {
            0 { append bin "0000" }
            1 { append bin "0001" }
            2 { append bin "0010" }
            3 { append bin "0011" }
            4 { append bin "0100" }
            5 { append bin "0101" }
            6 { append bin "0110" }
            7 { append bin "0111" }
            8 { append bin "1000" }
            9 { append bin "1001" }
            A { append bin "1010" }
            B { append bin "1011" }
            C { append bin "1100" }
            D { append bin "1101" }
            E { append bin "1110" }
            F { append bin "1111" }
        }
    }
    return $bin
}

proc parsePacket {bin {pos 0} {versionSum 0}} {
    if {[string length $bin] - $pos < 6} {
        return [list $versionSum $pos {}]
    }
    set version [scan [string range $bin $pos [expr {$pos+2}]] %b]
    set typeID [scan [string range $bin [expr {$pos+3}] [expr {$pos+5}]] %b]
    set pos [expr {$pos+6}]
    set versionSum [expr {$versionSum + $version}]
    set subPackets {}
    set value ""

    if {$typeID == 4} {
        set literalValue ""
        while 1 {
            if {[string length $bin] - $pos < 5} {
                return [list $versionSum $pos {}]
            }
            set group [string range $bin $pos [expr {$pos+4}]]
            set pos [expr {$pos+5}]
            append literalValue [string range $group 1 end]
            if {[string index $group 0] == "0"} {
                break
            }
        }
        set value [scan $literalValue %b]
    } else {
        if {[string length $bin] - $pos < 1} {
            return [list $versionSum $pos {}]
        }
        set lengthTypeID [string index $bin $pos]
        set pos [incr pos]
        if {$lengthTypeID == 0} {
            if {[string length $bin] - $pos < 15} {
                return [list $versionSum $pos {}]
            }
            set totalLength [scan [string range $bin $pos [expr {$pos+14}]] %b]
            set pos [incr pos 15]
            set endPos [expr {$pos + $totalLength}]
            while {$pos < $endPos} {
                lassign [parsePacket $bin $pos $versionSum] versionSum pos subPacket
                lappend subPackets $subPacket
            }
        } else {
            if {[string length $bin] - $pos < 11} {
                return [list $versionSum $pos {}]
            }
            set numSubPackets [scan [string range $bin $pos [expr {$pos+10}]] %b]
            set pos [incr pos 11]
            for {set i 0} {$i < $numSubPackets} {incr i} {
                lassign [parsePacket $bin $pos $versionSum] versionSum pos subPacket
                lappend subPackets $subPacket
            }
        }
        set value [calculateValue $typeID $subPackets]
    }
    return [list $versionSum $pos [list $version $typeID $value $subPackets]]
}

proc calculateValue {typeID subPackets} {
    switch $typeID {
        0 { # sum
            set sum 0
            foreach packet $subPackets {
                lassign $packet version typeID value subPackets
                set sum [expr {$sum + $value}]
            }
            return $sum
        }
        1 { # product
            set product 1
            foreach packet $subPackets {
                lassign $packet version typeID value subPackets
                set product [expr {$product * $value}]
            }
            return $product
        }
        2 { # minimum
            set min [lindex $subPackets 0 2]
            foreach packet $subPackets {
                lassign $packet version typeID value subPackets
                if {$value < $min} {
                    set min $value
                }
            }
            return $min
        }
        3 { # maximum
            set max [lindex $subPackets 0 2]
            foreach packet $subPackets {
                lassign $packet version typeID value subPackets
                if {$value > $max} {
                    set max $value
                }
            }
            return $max
        }
        5 { # greater than
            lassign [lindex $subPackets 0] version1 typeID1 value1 subPackets1
            lassign [lindex $subPackets 1] version2 typeID2 value2 subPackets2
            return [expr {$value1 > $value2}]
        }
        6 { # less than
            lassign [lindex $subPackets 0] version1 typeID1 value1 subPackets1
            lassign [lindex $subPackets 1] version2 typeID2 value2 subPackets2
            return [expr {$value1 < $value2}]
        }
        7 { # equal to
            lassign [lindex $subPackets 0] version1 typeID1 value1 subPackets1
            lassign [lindex $subPackets 1] version2 typeID2 value2 subPackets2
            return [expr {$value1 == $value2}]
        }
    }
}

# Main program
if {[file exists "input.txt"]} {
    set inputFile [open "input.txt" r]
    set hexData [read $inputFile]
    close $inputFile
    set hexData [string trim $hexData]
    set binData [hexToBin $hexData]
    lassign [parsePacket $binData] versionSum pos packet
    puts "Part 1: $versionSum"
    lassign $packet version typeID value subPackets
    puts "Part 2: $value"
} else {
    puts "Error: input.txt not found"
}
