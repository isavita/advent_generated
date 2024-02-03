
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $prev = 0;
my $current;
my $count = 0;

while (my $line = <$fh>) {
    chomp $line;
    $current = int($line);
    if ($prev != 0 && $current > $prev) {
        $count++;
    }
    $prev = $current;
}

close($fh);

print "$count\n";
