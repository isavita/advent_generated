use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my @vals;
while (my $line = <$fh>) {
    chomp($line);
    next if $line eq "";
    push @vals, $line;
}

close($fh);

my $prevSum = $vals[0] + $vals[1] + $vals[2];
my $count = 0;

for (my $i = 3; $i < scalar @vals; $i++) {
    my $currSum = $vals[$i-2] + $vals[$i-1] + $vals[$i];
    if ($currSum > $prevSum) {
        $count++;
    }
    $prevSum = $currSum;
}

print $count . "\n";