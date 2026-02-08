
set f [open "input.txt"]
set data [read $f]
close $f
set lines [split [string trim $data] "\n"]
set dirSizes [dict create]
dict set dirColors / 0
set curr {}
set sum 0

foreach line $lines {
    if {[string match "$ cd*" $line]} {
        set dir [lindex $line 2]
        if {$dir eq "/"} {
            set curr {}
        } elseif {$dir eq ".."} {
            if {[llength $curr] > 0} {
                set curr [lreplace $curr end end]
            }
        } else {
            lappend curr $dir
        }
        set path "/"
        foreach comp $curr {
            append path "/" $comp
        }
        if {![dict exists $dirSizes $path]} {
            dict set dirSizes $path 0
        }
    } elseif {[string range $line 0 5] ne "$ ls"} {
        set fields [split $line " "]
        set size [lindex $fields 0]
        if {$size ne "dir"} {
            set sz [expr {$size}]
            dict incr dirColors / $sz
            set path ""
            foreach comp $curr {
                append path "/" $comp
                dict incr dirColors $path $sz
            }
        }
    }
}

set sum 0
foreach {p sz} [dict get $dirColors] {
    if {$sz <= 100000} {set sum [expr {$sum + $sz}]}
}
puts $sum
