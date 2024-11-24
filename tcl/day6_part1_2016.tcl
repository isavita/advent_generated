
proc getMostCommonChar {count} {
    set maxCount 0
    set maxChar {}
    foreach {char cnt} $count {
        if {$cnt > $maxCount} {
            set maxCount $cnt
            set maxChar $char
        }
    }
    return $maxChar
}

proc getCorrectedMessage {messages} {
    if {[llength $messages] == 0} {return ""}
    set messageLength [string length [lindex $messages 0]]
    set count [lrepeat $messageLength [list]]

    for {set i 0} {$i < $messageLength} {incr i} {
        set countChar [dict create]
        foreach message $messages {
            set char [string index $message $i]
            dict incr countChar $char
        }
        lset count $i $countChar
    }

    set correctedMessage ""
    foreach charCount $count {
        append correctedMessage [getMostCommonChar $charCount]
    }
    return $correctedMessage
}


set f [open input.txt r]
set messages {}
while {[gets $f line] >= 0} {
    lappend messages $line
}
close $f

puts [getCorrectedMessage $messages]
