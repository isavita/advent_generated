
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $directions = <$fh>;
close($fh);

my %visitedHouses;
my ($x, $y) = (0, 0);

$visitedHouses{"$x,$y"} = 1;

foreach my $dir (split //, $directions) {
    if ($dir eq '^') {
        $y++;
    } elsif ($dir eq 'v') {
        $y--;
    } elsif ($dir eq '>') {
        $x++;
    } elsif ($dir eq '<') {
        $x--;
    }

    $visitedHouses{"$x,$y"} = 1;
}

print scalar(keys %visitedHouses) . "\n";
