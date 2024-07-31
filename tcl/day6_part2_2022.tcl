proc find_marker {buffer length} {
    set n [string length $buffer]
    for {set i 0} {$i <= $n - $length} {incr i} {
        set substring [string range $buffer $i [expr {$i + $length - 1}]]
        if {[llength [lsort -unique [split $substring ""]]] == $length} {
            return [expr {$i + $length}]
        }
    }
    return -1
}

set file [open "input.txt" r]
set datastream [read $file]
close $file

set start_of_packet [find_marker $datastream 4]
set start_of_message [find_marker $datastream 14]

puts "First start-of-packet marker after character: $start_of_packet"
puts "First start-of-message marker after character: $start_of_message"