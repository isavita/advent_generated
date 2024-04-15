use strict;
use warnings;

my @ground = (['+']);
my ($maxX, $minX, $maxY, $minY) = (0, 0, 0, 20);
my ($xOffset, $yOffset) = (500, 0);

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my $input = do { local $/; <$fh> };
close $fh;

my @lines = split /\n/, $input;

foreach my $line (@lines) {
    my @split = split /[=, .]+/, $line;
    if ($split[0] eq 'x') {
        my $x = $split[1] - $xOffset;
        my $y1 = $split[3] - $yOffset;
        my $y2 = $split[4] - $yOffset;

        while ($x >= $maxX) {
            $maxX++;
            foreach my $row (@ground) {
                push @$row, '.';
            }
        }
        while ($x <= $minX) {
            $minX--;
            foreach my $row (@ground) {
                unshift @$row, '.';
            }
        }
        while ($y2 > $maxY) {
            $maxY++;
            push @ground, [('.') x @{$ground[0]}];
        }
        $minY = $y1 if $y1 < $minY;
        foreach my $i ($y1 .. $y2) {
            $ground[$i][$x - $minX] = '#';
        }
    } else {
        my $y = $split[1] - $yOffset;
        my $x1 = $split[3] - $xOffset;
        my $x2 = $split[4] - $xOffset;

        while ($y > $maxY) {
            $maxY++;
            push @ground, [('.') x @{$ground[0]}];
        }
        while ($x2 >= $maxX) {
            $maxX++;
            foreach my $row (@ground) {
                push @$row, '.';
            }
        }
        while ($x1 <= $minX) {
            $minX--;
            foreach my $row (@ground) {
                unshift @$row, '.';
            }
        }
        foreach my $i ($x1 .. $x2) {
            $ground[$y][$i - $minX] = '#';
        }
        $minY = $y if $y < $minY;
    }
}

my ($waterCount, $flowCount) = (0, 0);
my $roundLimit = 200000;

while ($ground[1][-$minX] ne '|' && $waterCount < $roundLimit) {
    my $canMove = 1;
    my ($x, $y, $tryLeft) = (-$minX, 1, 0);
    while ($canMove) {
        if ($y + 1 > $maxY || $ground[$y + 1][$x] eq '|') {
            $ground[$y][$x] = '|';
            $canMove = 0;
            $flowCount++ if $y >= $minY;
        } elsif ($ground[$y + 1][$x] eq '.') {
            $y++;
            $tryLeft = 0;
        } elsif ($ground[$y + 1][$x] eq '#' || $ground[$y + 1][$x] eq '~') {
            if (($tryLeft == 1 && $ground[$y][$x - 1] eq '|') ||
                ($tryLeft == 2 && $ground[$y][$x + 1] eq '|') ||
                ($ground[$y][$x + 1] eq '|' && $ground[$y][$x - 1] ne '.') ||
                ($ground[$y][$x + 1] ne '.' && $ground[$y][$x - 1] eq '|')) {
                $ground[$y][$x] = '|';
                $flowCount++;
                $canMove = 0;
                for (my $i = $x + 1; $ground[$y][$i] eq '~'; $i++) {
                    $ground[$y][$i] = '|';
                    $waterCount--;
                    $flowCount++;
                }
                for (my $i = $x - 1; $ground[$y][$i] eq '~'; $i--) {
                    $ground[$y][$i] = '|';
                    $waterCount--;
                    $flowCount++;
                }
            } elsif (($tryLeft == 0 && $ground[$y][$x - 1] eq '.') ||
                     ($tryLeft == 1 && $ground[$y][$x - 1] eq '.')) {
                $x--;
                $tryLeft = 1;
            } elsif (($tryLeft == 0 && $ground[$y][$x + 1] eq '.') ||
                     ($tryLeft == 2 && $ground[$y][$x + 1] eq '.')) {
                $x++;
                $tryLeft = 2;
            } else {
                $canMove = 0;
                $ground[$y][$x] = '~';
                $waterCount++;
            }
        }
    }
}

print "$waterCount\n";