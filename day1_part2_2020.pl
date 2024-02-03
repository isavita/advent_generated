
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @expenses;
while (my $line = <$fh>) {
    chomp $line;
    push @expenses, $line;
}
close($fh);

for (my $i = 0; $i < scalar @expenses; $i++) {
    for (my $j = $i + 1; $j < scalar @expenses; $j++) {
        for (my $k = $j + 1; $k < scalar @expenses; $k++) {
            if ($expenses[$i] + $expenses[$j] + $expenses[$k] == 2020) {
                print $expenses[$i] * $expenses[$j] * $expenses[$k] . "\n";
                exit;
            }
        }
    }
}
