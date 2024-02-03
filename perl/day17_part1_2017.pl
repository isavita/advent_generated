
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
my $steps = <$fh>;
close $fh;

my @buffer = (0);
my $currentPos = 0;

for (my $i = 1; $i <= 2017; $i++) {
    $currentPos = ($currentPos + $steps) % scalar(@buffer);
    splice(@buffer, $currentPos + 1, 0, $i);
    $currentPos++;
}

my $result;
foreach my $i (0..$#buffer) {
    if ($buffer[$i] == 2017) {
        $result = $buffer[($i + 1) % scalar(@buffer)];
        last;
    }
}

print "$result\n";
