set freq 0
set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    set sign 1
    if {[string index $line 0] eq "-"} {
        set sign -1
        set line [string range $line 1 end]
    }
    set num [expr {$sign * [string trim $line]}]
    set freq [expr {$freq + $num}]
}
close $file
puts $freq