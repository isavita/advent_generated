#!/usr/bin/env tclsh
set nextId 0
array set name2id {}
array set adj {}
array set memo {}

proc getId {name} {
    global name2id nextId adj
    if {[info exists name2id($name)]} {
        return $name2id($name)
    }
    set id $nextId
    set name2id($name) $id
    set adj($id) {}
    incr nextId
    return $id
}

proc dfs {u target} {
    global adj memo
    if {$u == $target} {return 1}
    if {[info exists memo($u)]} {return $memo($u)}
    set total 0
    foreach v $adj($u) {
        set total [expr {$total + [dfs $v $target]}]
    }
    set memo($u) $total
    return $total
}

set f [open "input.txt"]
while {[gets $f line] >= 0} {
    if {[regexp {^([^:]+):\s*(.*)$} $line -> src rest]} {
        set u [getId $src]
        foreach tok [split $rest] {
            if {$tok eq ""} continue
            set v [getId $tok]
            lappend adj($u) $v
        }
    }
}
close $f

set start [getId "you"]
set finish [getId "out"]
puts [dfs $start $finish]