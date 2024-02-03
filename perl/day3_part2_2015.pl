
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $directions = <$fh>;
close($fh);

my %visitedHouses;
my ($xSanta, $ySanta) = (0, 0);
my ($xRobo, $yRobo) = (0, 0);
my $isSantaTurn = 1;

$visitedHouses{"$xSanta,$ySanta"} = 1;

for my $dir (split //, $directions) {
    my ($x, $y);
    if ($isSantaTurn) {
        ($x, $y) = (\$xSanta, \$ySanta);
    } else {
        ($x, $y) = (\$xRobo, \$yRobo);
    }

    if ($dir eq '^') {
        $$y++;
    } elsif ($dir eq 'v') {
        $$y--;
    } elsif ($dir eq '>') {
        $$x++;
    } elsif ($dir eq '<') {
        $$x--;
    }

    $visitedHouses{"$$x,$$y"} = 1;
    $isSantaTurn = !$isSantaTurn;
}

my $numVisitedHouses = keys %visitedHouses;
print "$numVisitedHouses\n";
