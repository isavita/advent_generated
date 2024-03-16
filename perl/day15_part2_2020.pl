use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";

my $data = <$fh>;
chomp $data;
my @startingNumbers = split(',', $data);

my %spoken;
my $lastSpoken;
foreach my $i (0..$#startingNumbers) {
    if ($i == $#startingNumbers) {
        $lastSpoken = $startingNumbers[$i];
    } else {
        $spoken{$startingNumbers[$i]} = $i + 1;
    }
}

for my $turn (scalar(@startingNumbers) + 1 .. 30000000) {
    my $nextNumber = 0; # Initialize $nextNumber to 0
    if (exists $spoken{$lastSpoken}) {
        $nextNumber = $turn - 1 - $spoken{$lastSpoken};
    }
    $spoken{$lastSpoken} = $turn - 1;
    $lastSpoken = $nextNumber;
}

print "$lastSpoken\n";

close($fh);