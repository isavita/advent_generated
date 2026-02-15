package require md5

set f [open "input.txt" r]
set doorId [string trim [read $f]]
close $f

set password ""
set index 0
while {[string length $password] < 8} {
    set hash [string tolower [md5::md5 -hex "${doorId}${index}"]]
    # string match avoids allocating a substring every iteration
    if {[string match 00000* $hash]} {
        append password [string index $hash 5]
    }
    incr index
}
puts $password
