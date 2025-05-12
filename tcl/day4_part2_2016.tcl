
proc is_real_room {name checksum} {
    set cleaned_name [string map {- ""} $name]
    array set counts {}
    foreach char [split $cleaned_name ""] {
        incr counts($char)
    }

    set char_counts {}
    foreach char [array names counts] {
        lappend char_counts [list $counts($char) $char]
    }

    set sorted_counts [lsort -command {apply {{a b} {
        set count_a [lindex $a 0]
        set char_a [lindex $a 1]
        set count_b [lindex $b 0]
        set char_b [lindex $b 1]
        if {$count_a != $count_b} {
            return [expr {$count_b - $count_a}]
        } else {
            return [string compare $char_a $char_b]
        }
    }}} $char_counts]

    set calculated_checksum ""
    foreach item [lrange $sorted_counts 0 4] {
        append calculated_checksum [lindex $item 1]
    }

    return [expr {$calculated_checksum eq $checksum}]
}

proc decrypt_name {name sector_id} {
    set decrypted ""
    set shift [expr {$sector_id % 26}]

    foreach char [split $name ""] {
        if {$char eq "-"} {
            append decrypted " "
        } else {
            set char_code [scan $char %c code]
            set base_code [scan a %c base]
            set shifted_code [expr {((($code - $base) + $shift) % 26) + $base}]
            append decrypted [format %c $shifted_code]
        }
    }
    return $decrypted
}

set total_sector_ids 0
set fid [open "input.txt" r]

while {[gets $fid line] >= 0} {
    if {[regexp {^([a-z-]+)-([0-9]+)\[([a-z]+)\]$} $line dummy name sector_id_str checksum]} {
        scan $sector_id_str %d sector_id

        if {[is_real_room $name $checksum]} {
            incr total_sector_ids $sector_id

            set decrypted [decrypt_name $name $sector_id]
            if {$decrypted eq "northpole object storage"} {
                puts $sector_id
            }
        }
    }
}

close $fid

puts $total_sector_ids
