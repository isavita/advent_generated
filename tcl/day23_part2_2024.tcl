set graph [dict create]
set nodes [list]

set f [open "input.txt" r]
while {[gets $f line] != -1} {
    set parts [split [string trim $line] "-"]
    set a [lindex $parts 0]
    set b [lindex $parts 1]
    
    dict lappend graph $a $b
    dict lappend graph $b $a
    
    if {$a ni $nodes} {lappend nodes $a}
    if {$b ni $nodes} {lappend nodes $b}
}
close $f

set best_clique [list]

proc bron_kerbosch {r p x} {
    global graph best_clique
    
    if {[llength $p] == 0 && [llength $x] == 0} {
        if {[llength $r] > [llength $best_clique]} {
            set best_clique $r
        }
        return
    }
    
    foreach v $p {
        set neighbors [dict get $graph $v]
        bron_kerbosch \
            [concat $r [list $v]] \
            [intersection $p $neighbors] \
            [intersection $x $neighbors]
        
        set p [lremove $p $v]
        lappend x $v
    }
}

proc intersection {list1 list2} {
    set result [list]
    foreach elem $list1 {
        if {$elem in $list2} {
            lappend result $elem
        }
    }
    return $result
}

proc lremove {list elem} {
    set idx [lsearch $list $elem]
    return [lreplace $list $idx $idx]
}

bron_kerbosch [list] $nodes [list]
set best_clique [lsort $best_clique]
puts [join $best_clique ","]