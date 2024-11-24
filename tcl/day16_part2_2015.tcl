
#!/usr/bin/env tclsh

# Target Sue characteristics
array set targetSue {
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
set fileContent [read $fp]
close $fp

# Process each line
foreach line [split $fileContent "\n"] {
    # Skip empty lines
    if {$line eq ""} continue

    # Parse Sue number and characteristics
    if {[regexp {Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)} $line -> sueNum thing1 amount1 thing2 amount2 thing3 amount3]} {
        # Create map of characteristics
        array set readingsMap [list $thing1 $amount1 $thing2 $amount2 $thing3 $amount3]
        
        set allRulesMatched 1

        # Special checks for cats and trees (greater than)
        foreach check {cats trees} {
            if {[info exists readingsMap($check)]} {
                if {$readingsMap($check) <= $targetSue($check)} {
                    set allRulesMatched 0
                    break
                }
                unset readingsMap($check)
            }
        }

        # Special checks for pomeranians and goldfish (less than)
        if {$allRulesMatched} {
            foreach check {pomeranians goldfish} {
                if {[info exists readingsMap($check)]} {
                    if {$readingsMap($check) >= $targetSue($check)} {
                        set allRulesMatched 0
                        break
                    }
                    unset readingsMap($check)
                }
            }
        }

        # Check remaining characteristics
        if {$allRulesMatched} {
            foreach {thing amount} [array get readingsMap] {
                if {$amount != $targetSue($thing)} {
                    set allRulesMatched 0
                    break
                }
            }
        }

        # If all rules match, print Sue number and exit
        if {$allRulesMatched} {
            puts $sueNum
            exit 0
        }

        # Clean up array
        array unset readingsMap
    }
}

# If no Sue found
puts "No matching Sue found"
