
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my $maxCalories = 0;
my $currentCalories = 0;

while (my $line = <$fh>) {
    chomp $line;

    if ($line eq "") {
        if ($currentCalories > $maxCalories) {
            $maxCalories = $currentCalories;
        }
        $currentCalories = 0;
        next;
    }

    my $calories = int($line);
    if ($calories eq '0' && $line ne '0') {
        die "Error converting line to int: $!";
    }

    $currentCalories += $calories;
}

if ($currentCalories > $maxCalories) {
    $maxCalories = $currentCalories;
}

close($fh);

print "$maxCalories\n";
