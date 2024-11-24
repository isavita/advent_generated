
#!/usr/bin/env tclsh

# Define the MFCSAM output
array set mfcsam {
    children 3 
    cats 7 
    samoyeds 2 
    pomeranians 3
    akitas 0 
    vizslas 0 
    goldfish 5 
    trees 3
    cars 2 
    perfumes 1
}

# Read input file
set fp [open "input.txt" r]
set file_data [read $fp]
close $fp

# Split file into lines
set lines [split $file_data "\n"]

# Process each line
foreach line $lines {
    # Skip empty lines
    if {$line eq ""} continue

    # Parse Sue number and attributes
    set parts [split $line " "]
    set sue_number [string trimright [lindex $parts 1] ":"]

    # Check if Sue matches MFCSAM output
    set matches 1
    for {set i 2} {$i < [llength $parts]} {incr i 2} {
        set item [string trimright [lindex $parts $i] ":"]
        set count [string trimright [lindex $parts [expr {$i+1}]] ","]

        if {[info exists mfcsam($item)] && $mfcsam($item) != $count} {
            set matches 0
            break
        }
    }

    # Print Sue number if matches
    if {$matches} {
        puts $sue_number
        break
    }
}
