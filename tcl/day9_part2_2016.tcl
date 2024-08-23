proc readFile {filename} {
    set file [open $filename r]
    set content [read $file]
    close $file
    return $content
}

proc decompressLength {data} {
    set length 0
    set i 0
    set dataLength [string length $data]

    while {$i < $dataLength} {
        if {[string index $data $i] eq "("} {
            set endMarker [string first ")" $data $i]
            if {$endMarker == -1} {
                break
            }
            set marker [string range $data $i+1 $endMarker-1]
            set parts [split $marker "x"]
            set charCount [lindex $parts 0]
            set repeatCount [lindex $parts 1]
            set i [expr {$endMarker + 1}]
            set subLength [decompressLength [string range $data $i [expr {$i + $charCount - 1}]]]
            set length [expr {$length + $subLength * $repeatCount}]
            set i [expr {$i + $charCount}]
        } else {
            set length [expr {$length + 1}]
            set i [expr {$i + 1}]
        }
    }
    return $length
}

set input [readFile "input.txt"]
set decompressedLength [decompressLength $input]
puts $decompressedLength