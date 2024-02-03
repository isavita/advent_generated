
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my @lanternFishCounts = (0) x 9;

while (my $line = <$fh>) {
    chomp $line;
    my @fishAges = split(',', $line);
    foreach my $age (@fishAges) {
        my $ageCount = int($age);
        $lanternFishCounts[$ageCount]++;
    }
}

for (my $i = 0; $i < 256; $i++) {
    my $newLanternFish = $lanternFishCounts[0];
    for (my $j = 0; $j < scalar(@lanternFishCounts) - 1; $j++) {
        $lanternFishCounts[$j] = $lanternFishCounts[$j + 1];
    }
    $lanternFishCounts[6] += $newLanternFish;
    $lanternFishCounts[8] = $newLanternFish;
}

my $total = 0;
foreach my $num (@lanternFishCounts) {
    $total += $num;
}

print "$total\n";
