package require md5

set f [open "input.txt" r]
set doorId [string trim [read $f]]
close $f

set password ""
set index 0
while {[string length $password] < 8} {
    set hash [md5::md5 -hex "${doorId}${index}"]
    if {[string range $hash 0 4] eq "00000"} {
        append password [string index $hash 5]
    }
    incr index
}

puts $password
