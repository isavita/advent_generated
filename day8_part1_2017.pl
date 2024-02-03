
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
my %registers;

while (my $line = <$fh>) {
    chomp $line;
    my @parts = split(' ', $line);
    my $reg = $parts[0];
    my $op = $parts[1];
    my $amount = $parts[2];
    my $condReg = $parts[4];
    my $condOp = $parts[5];
    my $condVal = $parts[6];

    my $cond = 0;
    if ($condOp eq '>') {
        $cond = $registers{$condReg} > $condVal;
    } elsif ($condOp eq '>=') {
        $cond = $registers{$condReg} >= $condVal;
    } elsif ($condOp eq '<') {
        $cond = $registers{$condReg} < $condVal;
    } elsif ($condOp eq '<=') {
        $cond = $registers{$condReg} <= $condVal;
    } elsif ($condOp eq '==') {
        $cond = $registers{$condReg} == $condVal;
    } elsif ($condOp eq '!=') {
        $cond = $registers{$condReg} != $condVal;
    }

    if ($cond) {
        if ($op eq 'inc') {
            $registers{$reg} += $amount;
        } elsif ($op eq 'dec') {
            $registers{$reg} -= $amount;
        }
    }
}

my $maxValue = 0;
foreach my $value (values %registers) {
    $maxValue = $value if $value > $maxValue;
}

print "$maxValue\n";
