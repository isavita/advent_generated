
use strict;
use warnings;

# Step 1: Read Input
open(my $fh, '<', "input.txt") or die "Could not open file 'input.txt' $!";
my @lines = <$fh>;
close($fh);

# Step 2: Initialize Registers
my %registers;

# Initialize highest value
my $highestValue = 0;

# Step 3: Process Instructions
foreach my $line (@lines) {
    my @parts = split(' ', $line);
    my $reg = $parts[0];
    my $op = $parts[1];
    my $amount = $parts[2];
    my $condReg = $parts[4];
    my $condOp = $parts[5];
    my $condVal = $parts[6];

    # Check condition
    my $cond = 0;
    if ($condOp eq ">") {
        $cond = $registers{$condReg} > $condVal;
    } elsif ($condOp eq ">=") {
        $cond = $registers{$condReg} >= $condVal;
    } elsif ($condOp eq "<") {
        $cond = $registers{$condReg} < $condVal;
    } elsif ($condOp eq "<=") {
        $cond = $registers{$condReg} <= $condVal;
    } elsif ($condOp eq "==") {
        $cond = $registers{$condReg} == $condVal;
    } elsif ($condOp eq "!=") {
        $cond = $registers{$condReg} != $condVal;
    }

    if ($cond) {
        if ($op eq "inc") {
            $registers{$reg} += $amount;
        } elsif ($op eq "dec") {
            $registers{$reg} -= $amount;
        }

        # Update highest value
        if ($registers{$reg} > $highestValue) {
            $highestValue = $registers{$reg};
        }
    }
}

# Step 4: Print the highest value
print "$highestValue\n";
