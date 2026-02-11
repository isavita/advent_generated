
set fp [open "input.txt" r]
set lines [split [string trim [read $fp]] "\n"]
close $fp

set h [llength $lines]
set w [string length [lindex $lines 0]]
set start_x [string first "." [lindex $lines 0]]
set end_x [string first "." [lindex $lines end]]
set start [list 0 $start_x]
set end [list [expr {$h-1}] $end_x]

set nodes [list $start $end]
for {set y 1} {$y < $h-1} {incr y} {
    set row [lindex $lines $y]
    for {set x 1} {$x < $w-1} {incr x} {
        if {[string index $row $x] eq "#"} continue
        set neighbors 0
        foreach d {{0 1} {0 -1} {1 0} {-1 0}} {
            set ny [expr {$y + [lindex $d 0]}]
            set nx [expr {$x + [lindex $d 1]}]
            if {[string index [lindex $lines $ny] $nx] ne "#"} { incr neighbors }
        }
        if {$neighbors > 2} { lappend nodes [list $y $x] }
    }
}

for {set i 0} {$i < [llength $nodes]} {incr i} {
    set n [lindex $nodes $i]
    set node_idx([lindex $n 0],[lindex $n 1]) $i
}

set adj_info [lrepeat [llength $nodes] {}]
for {set i 0} {$i < [llength $nodes]} {incr i} {
    set n [lindex $nodes $i]
    set sy [lindex $n 0]; set sx [lindex $n 1]
    foreach d {{0 1} {0 -1} {1 0} {-1 0}} {
        set cy [expr {$sy + [lindex $d 0]}]
        set cx [expr {$sx + [lindex $d 1]}]
        if {$cy < 0 || $cy >= $h || $cx < 0 || $cx >= $w || [string index [lindex $lines $cy] $cx] eq "#"} continue
        set dist 1
        set py $sy; set px $sx
        while {![info exists node_idx($cy,$cx)]} {
            set nny -1
            foreach d2 {{0 1} {0 -1} {1 0} {-1 0}} {
                set ty [expr {$cy + [lindex $d2 0]}]
                set tx [expr {$cx + [lindex $d2 1]}]
                if {($ty != $py || $tx != $px) && [string index [lindex $lines $ty] $tx] ne "#"} {
                    set nny $ty; set nnx $tx
                    break
                }
            }
            if {$nny == -1} break
            set py $cy; set px $cx
            set cy $nny; set cx $nnx
            incr dist
        }
        if {[info exists node_idx($cy,$cx)]} {
            set v $node_idx($cy,$cx)
            lset adj_info $i [linsert [lindex $adj_info $i] end [list $v [expr {1 << $v}] $dist]]
        }
    }
}

set max_dist 0
interp recursionlimit {} 10000

proc dfs {u mask dist} {
    global adj_info max_dist
    if {$u == 1} {
        if {$dist > $max_dist} { set max_dist $dist }
        return
    }
    foreach edge [lindex $adj_info $u] {
        set v_bit [lindex $edge 1]
        if {!($mask & $v_bit)} {
            dfs [lindex $edge 0] [expr {$mask | $v_bit}] [expr {$dist + [lindex $edge 2]}]
        }
    }
}

dfs 0 1 0
puts $max_dist

