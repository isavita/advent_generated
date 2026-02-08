#!/usr/bin/env wish

# Global adjacency list
array set ::neighbors {}

proc add_edge {u v} {
    upvar ::neighbors neighbors
    lappend neighbors($u) $v
}

proc dfs {cur tgt memoArr} {
    upvar ::neighbors neighbors
    upvar $memoArr memo
    if {$cur eq $tgt} { return 1 }
    if {[info exists memo($cur)]} { return $memo($cur) }
    set sum 0
    if {[info exists neighbors($cur)]} {
        foreach nb $neighbors($cur) {
            incr sum [dfs $nb $tgt $memoArr]
        }
    }
    set memo($cur) $sum
    return $sum
}

proc count_paths {src tgt} {
    upvar ::neighbors neighbors
    array set memo {}
    set res [dfs $src $tgt memo]
    array unset memo
    return $res
}

proc main {} {
    set f [open "input.txt" r]
    while {[gets $f line] >= 0} {
        set line [string trim $line]
        if {$line eq ""} continue
        set colon [string first ":" $line]
        if {$colon eq "-1"} continue
        set src [string trim [string range $line 0 [expr {$colon-1}]]]
        set dst [string trim [string range $line [expr {$colon+1}] end]]
        foreach tok [split $dst] {
            add_edge $src $tok
        }
    }
    close $f

    set s1 [expr {[count_paths "svr" "dac"] * [count_paths "dac" "fft"] * [count_paths "fft" "out"]}]
    set s2 [expr {[count_paths "svr" "fft"] * [count_paths "fft" "dac"] * [count_paths "dac" "out"]}]

    puts "Paths (svr->dac->fft->out): $s1"
    puts "Paths (svr->fft->dac->out): $s2"
    puts "Total paths visiting both: [expr {$s1 + $s2}]"
}

main
