proc readInput {filename} {
    set file [open $filename r]
    set input [gets $file]
    close $file
    return $input
}

proc dragonCurve {a} {
    set b [string reverse $a]
    set b [string map {"0" "1" "1" "0"} $b]
    return "${a}0${b}"
}

proc generateData {initialState length} {
    set data $initialState
    while {[string length $data] < $length} {
        set data [dragonCurve $data]
    }
    return [string range $data 0 [expr {$length - 1}]]
}

proc calculateChecksum {data} {
    set checksum $data
    while {[expr {[string length $checksum] % 2 == 0}]} {
        set newChecksum ""
        for {set i 0} {$i < [string length $checksum]} {incr i 2} {
            set pair [string range $checksum $i [expr {$i + 1}]]
            append newChecksum [expr {[string index $pair 0] == [string index $pair 1]}]
        }
        set checksum $newChecksum
    }
    return $checksum
}

set initialState [readInput "input.txt"]
set diskLength 272
set data [generateData $initialState $diskLength]
set checksum [calculateChecksum $data]
puts $checksum