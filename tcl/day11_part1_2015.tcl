
proc incrementPassword {pass} {
    set len [string length $pass]
    set i [expr {$len - 1}]
    while {$i >= 0} {
        set char [string index $pass $i]
        if {$char eq "z"} {
            set pass [string replace $pass $i $i "a"]
            incr i -1
        } else {
            set code [scan $char %c]
            set next_char [format %c [expr {$code + 1}]]
            set pass [string replace $pass $i $i $next_char]
            break
        }
    }
    return $pass
}

proc hasIncreasingStraight {pass} {
    set len [string length $pass]
    for {set i 0} {$i <= $len - 3} {incr i} {
        set c1 [string index $pass $i]
        set c2 [string index $pass [expr {$i+1}]]
        set c3 [string index $pass [expr {$i+2}]]
        set code1 [scan $c1 %c]
        set code2 [scan $c2 %c]
        set code3 [scan $c3 %c]
        if {$code2 == $code1 + 1 && $code3 == $code2 + 1} {
            return 1
        }
    }
    return 0
}

proc containsForbiddenLetters {pass} {
    return [regexp {[iol]} $pass]
}

proc hasTwoPairs {pass} {
    set pairs 0
    set i 0
    set len [string length $pass]
    while {$i < $len - 1} {
        set c1 [string index $pass $i]
        set c2 [string index $pass [expr {$i+1}]]
        if {$c1 eq $c2} {
            incr pairs
            incr i 2
        } else {
            incr i
        }
    }
    return [expr {$pairs >= 2}]
}

proc findNextPassword {pass} {
    set current_pass $pass
    while {1} {
        set current_pass [incrementPassword $current_pass]
        if {[hasIncreasingStraight $current_pass] &&
            ![containsForbiddenLetters $current_pass] &&
            [hasTwoPairs $current_pass]} {
            return $current_pass
        }
    }
}

proc main {} {
    set file_id [open "input.txt" r]
    set input_password [string trim [read $file_id]]
    close $file_id

    set next_password [findNextPassword $input_password]
    puts $next_password
}

main
