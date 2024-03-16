use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my @grid;
while (my $line = <$fh>) {
    chomp $line;
    push @grid, [split('', $line)];
}

my ($x, $y) = (0, 0);
for my $i (0..$#{$grid[0]}) {
    if ($grid[0][$i] eq '|') {
        $x = $i;
        last;
    }
}

my ($dx, $dy) = (0, 1);
my @letters;

while (1) {
    last if $x < 0 || $x >= @{$grid[0]} || $y < 0 || $y >= @grid;

    my $cell = $grid[$y][$x];

    last if $cell eq ' ';

    push @letters, $cell if $cell ge 'A' && $cell le 'Z';

    if ($cell eq '+') {
        if ($dx == 0) {
            if ($x > 0 && ($grid[$y][$x-1] eq '-' || ($grid[$y][$x-1] ge 'A' && $grid[$y][$x-1] le 'Z'))) {
                ($dx, $dy) = (-1, 0);
            } else {
                ($dx, $dy) = (1, 0);
            }
        } else {
            if ($y > 0 && ($grid[$y-1][$x] eq '|' || ($grid[$y-1][$x] ge 'A' && $grid[$y-1][$x] le 'Z'))) {
                ($dx, $dy) = (0, -1);
            } else {
                ($dx, $dy) = (0, 1);
            }
        }
    }

    $x += $dx;
    $y += $dy;
}

print join('', @letters);