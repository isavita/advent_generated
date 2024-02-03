
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @fishes = (0) x 9;

while (my $line = <$fh>) {
    chomp $line;
    my @fishStrs = split(',', $line);
    foreach my $fishStr (@fishStrs) {
        my $fish = int($fishStr);
        $fishes[$fish]++;
    }
}

for (my $day = 1; $day <= 80; $day++) {
    my $newFish = $fishes[0];
    for (my $i = 1; $i < scalar(@fishes); $i++) {
        $fishes[$i-1] = $fishes[$i];
    }
    $fishes[6] += $newFish;
    $fishes[8] = $newFish;
}

my $totalFish = 0;
foreach my $fish (@fishes) {
    $totalFish += $fish;
}

print "$totalFish\n";
