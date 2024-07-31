proc readInput {filename} {
    set file [open $filename r]
    set line [gets $file]
    close $file
    return $line
}

proc nextSequence {sequence} {
    set result ""
    set i 0
    set length [string length $sequence]
    while {$i < $length} {
        set count 1
        set digit [string index $sequence $i]
        while {$i + 1 < $length && [string index $sequence [expr {$i + 1}]] eq $digit} {
            incr count
            incr i
        }
        append result "$count$digit"
        incr i
    }
    return $result
}

proc lookAndSay {sequence iterations} {
    for {set i 0} {$i < $iterations} {incr i} {
        set sequence [nextSequence $sequence]
    }
    return $sequence
}

set initialSequence [readInput "input.txt"]
set result [lookAndSay $initialSequence 40]
puts [string length $result]