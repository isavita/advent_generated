set validCount 0

# Open the input file
set fileId [open "input.txt" r]

# Read each line from the file
while {[gets $fileId line] >= 0} {
    # Split the line into words
    set words [split $line]

    # Use a set to track unique words
    set wordSet {}

    set isValid 1
    foreach word $words {
        if {[lsearch -exact $wordSet $word] != -1} {
            set isValid 0
            break
        }
        lappend wordSet $word
    }

    # Increment valid count if the passphrase is valid
    if {$isValid} {
        incr validCount
    }
}

# Close the file
close $fileId

# Print the count of valid passphrases
puts $validCount