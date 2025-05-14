
package require Tcl 8.6

proc main {} {
    set filename "input.txt"
    set fid [open $filename r]
    set fs [dict create]
    dict set fs / type dir
    dict set fs / size 0

    set current_path "/"

    while {[gets $fid line] != -1} {
        set parts [split $line " "]
        set p0 [lindex $parts 0]
        set p1 [lindex $parts 1]

        if {$p0 eq "\$"} {
            if {$p1 eq "cd"} {
                set arg [lindex $parts 2]
                if {$arg eq "/"} {
                    set current_path "/"
                } elseif {$arg eq ".."} {
                    set current_path [file dirname $current_path]
                } else {
                    set current_path [file join $current_path $arg]
                }
            }
        } elseif {$p0 eq "dir"} {
            set dirname [lindex $parts 1]
            set dir_path [file join $current_path $dirname]
            if {![dict exists $fs $dir_path]} {
                 dict set fs $dir_path type dir
                 dict set fs $dir_path size 0
            }
        } else { ;# file
            set size $p0
            set filename [lindex $parts 1]
            set file_path [file join $current_path $filename]
            if {![dict exists $fs $file_path]} {
                dict set fs $file_path type file
                dict set fs $file_path size $size

                set temp_path $current_path
                while {1} {
                    set current_dir_size [dict get $fs $temp_path size]
                    dict set fs $temp_path size [expr {$current_dir_size + $size}]
                    set parent_path [file dirname $temp_path]
                    if {$parent_path eq $temp_path} { break }
                    set temp_path $parent_path
                }
            }
        }
    }
    close $fid

    set dir_sizes [list]
    dict for {path info} $fs {
        if {[dict get $info type] eq "dir"} {
            lappend dir_sizes [dict get $info size]
        }
    }

    set sum_small_dirs 0
    foreach size $dir_sizes {
        if {$size <= 100000} {
            incr sum_small_dirs $size
        }
    }
    puts $sum_small_dirs

    set total_disk 70000000
    set required_space 30000000
    set total_fs_size [dict get $fs / size]
    set available_space [expr {$total_disk - $total_fs_size}]
    set need_space [expr {$required_space - $available_space}]

    set smallest_delete_size -1
    foreach size $dir_sizes {
        if {$size >= $need_space} {
            if {$smallest_delete_size == -1 || $size < $smallest_delete_size} {
                 set smallest_delete_size $size
            }
        }
    }
    puts $smallest_delete_size
}

main
