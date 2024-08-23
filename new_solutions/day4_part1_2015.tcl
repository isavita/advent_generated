package require md5

set file [open "input.txt" r]
set secretKey [gets $file]
close $file

set number 1
while {1} {
    set hash [md5::md5 -hex "$secretKey$number"]
    if {[string range $hash 0 4] eq "00000"} {
        puts $number
        break
    }
    incr number
}