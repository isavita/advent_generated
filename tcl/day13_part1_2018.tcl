
proc move_cart {track cart} {
    set x [lindex $cart 0]
    set y [lindex $cart 1]
    set dir [lindex $cart 2]
    set turn [lindex $cart 3]
    
    switch $dir {
        ">" {
            set nx [expr {$x + 1}]
            set track_char [lindex [lindex $track $y] $nx]
            switch $track_char {
                "\\" { set dir "v" }
                "/" { set dir "^" }
                "+" {
                    switch $turn {
                        0 { set dir "^"; set turn 1 }
                        1 { set turn 2 }
                        2 { set dir "v"; set turn 0 }
                    }
                }
            }
            return [list $nx $y $dir $turn]
        }
        "<" {
            set nx [expr {$x - 1}]
            set track_char [lindex [lindex $track $y] $nx]
            switch $track_char {
                "/" { set dir "v" }
                "\\" { set dir "^" }
                "+" {
                    switch $turn {
                        0 { set dir "v"; set turn 1 }
                        1 { set turn 2 }
                        2 { set dir "^"; set turn 0 }
                    }
                }
            }
            return [list $nx $y $dir $turn]
        }
        "^" {
            set ny [expr {$y - 1}]
            set track_char [lindex [lindex $track $ny] $x]
            switch $track_char {
                "/" { set dir ">" }
                "\\" { set dir "<" }
                "+" {
                    switch $turn {
                        0 { set dir "<"; set turn 1 }
                        1 { set turn 2 }
                        2 { set dir ">"; set turn 0 }
                    }
                }
            }
            return [list $x $ny $dir $turn]
        }
        "v" {
            set ny [expr {$y + 1}]
            set track_char [lindex [lindex $track $ny] $x]
            switch $track_char {
                "/" { set dir "<" }
                "\\" { set dir ">" }
                "+" {
                    switch $turn {
                        0 { set dir ">"; set turn 1 }
                        1 { set turn 2 }
                        2 { set dir "<"; set turn 0 }
                    }
                }
            }
            return [list $x $ny $dir $turn]
        }
    }
}

set track {}
set carts {}
set y 0

set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    set row {}
    set x 0
    foreach char [split $line ""] {
        switch $char {
            ">" { lappend row "-"; lappend carts [list $x $y ">" 0] }
            "<" { lappend row "-"; lappend carts [list $x $y "<" 0] }
            "^" { lappend row "|"; lappend carts [list $x $y "^" 0] }
            "v" { lappend row "|"; lappend carts [list $x $y "v" 0] }
            default { lappend row $char }
        }
        incr x
    }
    lappend track $row
    incr y
}
close $file

set collision 0
while {!$collision} {
    set new_carts {}
    foreach cart $carts {
        lappend new_carts [move_cart $track $cart]
    }
    set carts $new_carts
    
    set len [llength $carts]
    for {set i 0} {$i < $len} {incr i} {
        set cart1 [lindex $carts $i]
        for {set j [expr {$i + 1}]} {$j < $len} {incr j} {
            set cart2 [lindex $carts $j]
            if {[lindex $cart1 0] == [lindex $cart2 0] && [lindex $cart1 1] == [lindex $cart2 1]} {
                puts "[lindex $cart1 0],[lindex $cart1 1]"
                set collision 1
                break
            }
        }
        if {$collision} break
    }
}
