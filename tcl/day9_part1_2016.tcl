set file [open "input.txt" r]
set input [read $file]
close $file

proc getDecompressedLength {input} {
    set length 0
    set i 0
    set pattern {\(\d+x\d+\)}
    while {$i < [string length $input]} {
        if {[regexp $pattern [string range $input $i end] match]} {
            regexp {\((\d+)x(\d+)\)} $match -> charCount repeatCount
            set charCount [expr {$charCount}]
            set repeatCount [expr {$repeatCount}]
            set i [expr {$i + [string length $match] + $charCount}]
            set length [expr {$length + $charCount * $repeatCount}]
        } else {
            set length [expr {$length + 1}]
            set i [expr {$i + 1}]
        }
    }
    return $length
}

puts [getDecompressedLength $input]