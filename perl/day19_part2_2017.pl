#!/usr/bin/perl

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my @grid;

while (my $line = <$fh>) {
    chomp $line;
    push @grid, [split //, $line];
}

my ($x, $y) = (0, 0);

for my $i (0..$#{$grid[0]}) {
    if ($grid[0][$i] eq '|') {
        $x = $i;
        last;
    }
}

my ($dx, $dy) = (0, 1);
my $steps = 0;

while (1) {
    last if $x < 0 or $x >= @{$grid[0]} or $y < 0 or $y >= @grid;

    my $cell = $grid[$y][$x];

    last if $cell eq ' ';

    $steps++;

    if ($cell eq '+') {
        if ($dx == 0) {
            if ($x > 0 and ($grid[$y][$x-1] eq '-' or ($grid[$y][$x-1] ge 'A' and $grid[$y][$x-1] le 'Z'))) 
            {
                ($dx, $dy) = (-1, 0);
            } else {
                ($dx, $dy) = (1, 0);
            }
        } else {
            if ($y > 0 and ($grid[$y-1][$x] eq '|' or ($grid[$y-1][$x] ge 'A' and $grid[$y-1][$x] le 'Z'))) 
            {
                ($dx, $dy) = (0, -1);
            } else {
                ($dx, $dy) = (0, 1);
            }
        }
    }

    $x += $dx;
    $y += $dy;
}

print "$steps\n";