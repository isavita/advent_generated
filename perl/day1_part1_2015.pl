
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $input = do { local $/; <$fh> };
close($fh);

my $floor = 0;
for my $c (split //, $input) {
    if ($c eq '(') {
        $floor++;
    } else {
        $floor--;
    }
}

print "$floor\n";
