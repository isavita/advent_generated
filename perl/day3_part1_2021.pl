
open(my $fh, '<', 'input.txt') or die $!;
my @counts;
for my $i (0..11) {
    $counts[$i][0] = 0;
    $counts[$i][1] = 0;
}

while (my $line = <$fh>) {
    chomp $line;
    my @nums = split('', $line);
    for my $i (0..$#nums) {
        $counts[$i][$nums[$i]]++;
    }
}

my $gammaRate = 0;
my $epsilonRate = 0;
for my $i (0..11) {
    if ($counts[$i][0] > $counts[$i][1]) {
        $gammaRate |= 1 << (11 - $i);
    } else {
        $epsilonRate |= 1 << (11 - $i);
    }
}

print $gammaRate * $epsilonRate;
