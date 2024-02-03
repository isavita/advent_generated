
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @instructions;
while (my $line = <$fh>) {
    chomp $line;
    push @instructions, $line;
}
close($fh);

my @keypad = (
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
);
my ($x, $y) = (1, 1);
my $code = '';

foreach my $instruction (@instructions) {
    foreach my $move (split //, $instruction) {
        if ($move eq 'U') {
            $x-- if $x > 0;
        } elsif ($move eq 'D') {
            $x++ if $x < 2;
        } elsif ($move eq 'L') {
            $y-- if $y > 0;
        } elsif ($move eq 'R') {
            $y++ if $y < 2;
        }
    }
    $code .= $keypad[$x][$y];
}

print $code . "\n";
