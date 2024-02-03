
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error reading file: $!";
my $input = <$fh>;
close($fh);

chomp($input);
my @startingNumbers = split(',', $input);

my %lastSpoken;
my ($lastNumber, $nextNumber);

for (my $turn = 1; $turn <= 2020; $turn++) {
    if ($turn - 1 < scalar @startingNumbers) {
        $lastNumber = $startingNumbers[$turn - 1];
        $lastSpoken{$lastNumber} = $turn;
        next;
    }
    if (exists $lastSpoken{$lastNumber} && $lastSpoken{$lastNumber} != $turn - 1) {
        $nextNumber = $turn - 1 - $lastSpoken{$lastNumber};
    } else {
        $nextNumber = 0;
    }
    $lastSpoken{$lastNumber} = $turn - 1;
    $lastNumber = $nextNumber;
}

print "$lastNumber\n";
