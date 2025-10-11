
set key_pad {789 456 123 " 0A"}
set robot_pad {" ^A" "<v>"}
set MEMO_SIZE 65536
set MAX_ROBOTS 26
array set memo {}

proc hash {str num} {
    set h 5381
    foreach c [split $str ""] {
        set h [expr {($h << 5) + $h + [scan $c %c]}]
    }
    set h [expr {$h ^ $num}]
    return [expr {$h & ($::MEMO_SIZE - 1)}]
}

proc find_pos {mat ch} {
    set r 0
    foreach row $mat {
        set c [string first $ch $row]
        if {$c != -1} {return [list $r $c]}
        incr r
    }
    return [list -1 -1]
}

proc is_ok {mat rows cols r c seq} {
    foreach ch [split $seq ""] {
        if {$r < 0 || $r >= $rows || $c < 0 || $c >= $cols ||
            [string index [lindex $mat $r] $c] eq " "} {
            return 0
        }
        switch $ch {
            ^ {incr r -1}
            v {incr r}
            < {incr c -1}
            > {incr c}
        }
    }
    return 1
}

proc gen_moves {r c obj mat rows cols} {
    lassign [find_pos $mat $obj] tr tc
    set buf ""
    if {$c > $tc} {append buf [string repeat "<" [expr {$c - $tc}]]}
    if {$r > $tr} {append buf [string repeat "^" [expr {$r - $tr}]]}
    if {$r < $tr} {append buf [string repeat "v" [expr {$tr - $r}]]}
    if {$c < $tc} {append buf [string repeat ">" [expr {$tc - $c}]]}
    if {![is_ok $mat $rows $cols $r $c $buf]} {
        set buf ""
        if {$c < $tc} {append buf [string repeat ">" [expr {$tc - $c}]]}
        if {$r > $tr} {append buf [string repeat "^" [expr {$r - $tr}]]}
        if {$r < $tr} {append buf [string repeat "v" [expr {$tr - $r}]]}
        if {$c > $tc} {append buf [string repeat "<" [expr {$c - $tc}]]}
    }
    return $buf
}

proc solve {code robots} {
    set key "$code,$robots"
    if {[info exists ::memo($key)]} {return $::memo($key)}
    if {$robots <= 0} {
        set ::memo($key) [string length $code]
        return $::memo($key)
    }
    set ret 0
    if {$robots == $::MAX_ROBOTS} {
        set current_r 3
        set current_c 2
        set pad $::key_pad
        set pad_rows 4
        set pad_cols 3
    } else {
        set current_r 0
        set current_c 2
        set pad $::robot_pad
        set pad_rows 2
        set pad_cols 3
    }
    foreach ch [split $code ""] {
        set moves [gen_moves $current_r $current_c $ch $pad $pad_rows $pad_cols]
        lassign [find_pos $pad $ch] current_r current_c
        append moves A
        set ret [expr {$ret + [solve $moves [expr {$robots - 1}]]}]
    }
    set ::memo($key) $ret
    return $ret
}

set f [open "input.txt" r]
set total 0
while {[gets $f line] >= 0} {
    set line [string trim $line]
    if {$line eq ""} continue
    set numeric [regsub {\D} $line ""]
    set total [expr {$total + [solve $line $::MAX_ROBOTS] * $numeric}]
}
close $f
puts $total
