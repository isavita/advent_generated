proc main {} {
    set file [open "input.txt" r]
    set moves [split [read $file] ","]
    close $file

    set programs [split "abcdefghijklmnop" ""]
    
    foreach move $moves {
        switch -regexp -- $move {
            {s(\d+)} {
                set x [string trimleft $move "s"]
                set programs [spin $programs $x]
            }
            {x(\d+)/(\d+)} {
                regexp {x(\d+)/(\d+)} $move -> a b
                set programs [exchange $programs $a $b]
            }
            {p([a-p])/([a-p])} {
                regexp {p([a-p])/([a-p])} $move -> a b
                set programs [partner $programs $a $b]
            }
        }
    }
    puts [join $programs ""]
}

proc spin {programs x} {
    set x [expr {$x % [llength $programs]}]
    return [concat [lrange $programs end-[expr {$x-1}] end] [lrange $programs 0 end-$x]]
}

proc exchange {programs a b} {
    set temp [lindex $programs $a]
    set programs [lreplace $programs $a $a [lindex $programs $b]]
    set programs [lreplace $programs $b $b $temp]
    return $programs
}

proc partner {programs a b} {
    set indexA [lsearch -exact $programs $a]
    set indexB [lsearch -exact $programs $b]
    return [exchange $programs $indexA $indexB]
}

main