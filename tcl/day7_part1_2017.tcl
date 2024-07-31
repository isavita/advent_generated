set file [open "input.txt" r]
set data [read $file]
close $file

array set holderMap {}
array set heldMap {}
set lines [split $data "\n"]

foreach line $lines {
    set names [regexp -all -inline {[a-z]+} $line]
    set holder [lindex $names 0]
    set holderMap($holder) 1

    if {[llength $names] > 1} {
        foreach name [lrange $names 1 end] {
            set heldMap($name) 1
        }
    }
}

foreach holder [array names holderMap] {
    if {[expr {![info exists heldMap($holder)]}]} {
        puts $holder
        break
    }
}