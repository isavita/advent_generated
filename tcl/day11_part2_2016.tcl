
set mats [dict create]
set n_mat 0

proc get_id {name} {
    global mats n_mat
    if {[dict exists $mats $name]} { return [dict get $mats $name] }
    dict set mats $name $n_mat
    incr n_mat
    return [expr {$n_mat - 1}]
}

proc is_valid {mask} {
    set gens [expr {$mask & 0x3FF}]
    set chips [expr {$mask >> 10}]
    return [expr {$gens == 0 || ($chips & ~$gens) == 0}]
}

proc get_key {elev floors} {
    global n_mat
    set pairs {}
    for {set i 0} {$i < $n_mat} {incr i} {
        set g_bit [expr {1 << $i}]
        set c_bit [expr {1 << ($i + 10)}]
        set g_loc -1; set c_loc -1
        for {set f 0} {$f < 4} {incr f} {
            set m [lindex $floors $f]
            if {$m & $g_bit} { set g_loc $f }
            if {$m & $c_bit} { set c_loc $f }
        }
        lappend pairs "$g_loc,$c_loc"
    }
    return "$elev:[join [lsort $pairs] :]"
}

set fp [open "input.txt" r]
set data [read $fp]
close $fp

set floors {0 0 0 0}
set f_idx 0

foreach line [split $data "\n"] {
    if {[string trim $line] eq ""} continue
    regsub -all -- {-} $line " " line
    regsub -all -- {[.,]} $line "" line
    set tokens [split $line " "]
    set len [llength $tokens]
    for {set i 0} {$i < $len} {incr i} {
        set t [lindex $tokens $i]
        if {$t eq "generator"} {
            lset floors $f_idx [expr {[lindex $floors $f_idx] | (1 << [get_id [lindex $tokens [expr {$i-1}]]])}]
        } elseif {$t eq "microchip"} {
            lset floors $f_idx [expr {[lindex $floors $f_idx] | (1 << ([get_id [lindex $tokens [expr {$i-2}]]] + 10))}]
        }
    }
    incr f_idx
    if {$f_idx >= 4} break
}

# Part 2 additions
set e_id [get_id "elerium"]
set d_id [get_id "dilithium"]
lset floors 0 [expr {[lindex $floors 0] | (1 << $e_id) | (1 << ($e_id + 10))}]
lset floors 0 [expr {[lindex $floors 0] | (1 << $d_id) | (1 << ($d_id + 10))}]

set target_mask 0
for {set i 0} {$i < $n_mat} {incr i} {
    set target_mask [expr {$target_mask | (1 << $i) | (1 << ($i + 10))}]
}

set queue [list [list 0 $floors 0]]
set visited [dict create]
dict set visited [get_key 0 $floors] 1
set q_ptr 0

while {$q_ptr < [llength $queue]} {
    lassign [lindex $queue $q_ptr] elev curr_floors steps
    incr q_ptr
    
    if {[lindex $curr_floors 3] == $target_mask} {
        puts $steps
        exit
    }
    
    set mask [lindex $curr_floors $elev]
    set items {}
    for {set i 0} {$i < 20} {incr i} {
        if {$mask & (1 << $i)} { lappend items $i }
    }
    
    set dirs {}
    if {$elev < 3} { lappend dirs 1 }
    if {$elev > 0} { lappend dirs -1 }
    
    foreach d $dirs {
        set ne [expr {$elev + $d}]
        foreach i1 $items {
            set m1 [expr {1 << $i1}]
            # Move 1
            set nf $curr_floors
            lset nf $elev [expr {[lindex $nf $elev] ^ $m1}]
            lset nf $ne [expr {[lindex $nf $ne] ^ $m1}]
            
            if {[is_valid [lindex $nf $elev]] && [is_valid [lindex $nf $ne]]} {
                set k [get_key $ne $nf]
                if {![dict exists $visited $k]} {
                    dict set visited $k 1
                    lappend queue [list $ne $nf [expr {$steps + 1}]]
                }
            }
            
            # Move 2
            foreach i2 $items {
                if {$i2 <= $i1} continue
                set m2 [expr {$m1 | (1 << $i2)}]
                
                set nf2 $curr_floors
                lset nf2 $elev [expr {[lindex $nf2 $elev] ^ $m2}]
                lset nf2 $ne [expr {[lindex $nf2 $ne] ^ $m2}]
                
                if {[is_valid [lindex $nf2 $elev]] && [is_valid [lindex $nf2 $ne]]} {
                    set k [get_key $ne $nf2]
                    if {![dict exists $visited $k]} {
                        dict set visited $k 1
                        lappend queue [list $ne $nf2 [expr {$steps + 1}]]
                    }
                }
            }
        }
    }
}
