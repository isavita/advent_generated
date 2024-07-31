set file [open "input.txt" r]
set input [read $file]
close $file

set nice 0
set disallowPattern {ab|cd|pq|xy}

foreach line [split $input "\n"] {
    set vowels 0
    set hasDouble 0
    set prevChar ""

    foreach char [split $line ""] {
        if {[string first $char "aeiou"] >= 0} {
            incr vowels
        }
        if {$char eq $prevChar} {
            set hasDouble 1
        }
        set prevChar $char
    }

    if {$vowels >= 3 && ![regexp $disallowPattern $line] && $hasDouble} {
        incr nice
    }
}

puts $nice