proc dragonCurve {data length} {
    while {[string length $data] < $length} {
        set b [string reverse $data]
        set b [string map {0 1 1 0} $b]
        set data "${data}0${b}"
    }
    return [string range $data 0 [expr {$length - 1}]]
}

proc calculateChecksum {data} {
    while {[expr {[string length $data] % 2 == 0}]} {
        set checksum ""
        for {set i 0} {$i < [string length $data]} {incr i 2} {
            if {[string index $data $i] eq [string index $data [expr {$i + 1}]]} {
                append checksum 1
            } else {
                append checksum 0
            }
        }
        set data $checksum
    }
    return $data
}

set file [open "input.txt" r]
set initialState [gets $file]
close $file

set diskLength 35651584
set data [dragonCurve $initialState $diskLength]
set checksum [calculateChecksum $data]
puts $checksum