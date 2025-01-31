
use strict;
use warnings;
use feature 'say';

my $file = 'input.txt';
open(my $fh, '<', $file) or die "Could not open file '$file' $!";

my @grid;
while (my $line = <$fh>) {
    chomp $line;
    push @grid, $line;
}

my $h = scalar @grid;
my $w = length $grid[0];
my ($S, $E);
my @trackCells;
my @walls;

for my $i (0 .. $h - 1) {
    for my $j (0 .. $w - 1) {
        my $ch = substr($grid[$i], $j, 1);
        if ($ch eq 'S') {
            $S = [$i, $j];
        } elsif ($ch eq 'E') {
            $E = [$i, $j];
        }
        if ($ch eq '#') {
            $walls[$i][$j] = 1;
        } else {
            push @trackCells, [$i, $j];
        }
    }
}

my @dirs = ([1, 0], [-1, 0], [0, 1], [0, -1]);

sub normalDistFrom {
    my ($start) = @_;
    my @dist;
    for my $i (0 .. $h - 1) {
        for my $j (0 .. $w - 1) {
            $dist[$i][$j] = -1;
        }
    }
    $dist[$start->[0]][$start->[1]] = 0;
    my @q = ($start);
    while (@q) {
        my $cur = shift @q;
        for my $d (@dirs) {
            my ($nx, $ny) = ($cur->[0] + $d->[0], $cur->[1] + $d->[1]);
            next if $nx < 0 || $nx >= $h || $ny < 0 || $ny >= $w;
            next if $walls[$nx][$ny];
            if ($dist[$nx][$ny] == -1) {
                $dist[$nx][$ny] = $dist[$cur->[0]][$cur->[1]] + 1;
                push @q, [$nx, $ny];
            }
        }
    }
    return \@dist;
}

my $distFromS = normalDistFrom($S);
my $distFromE = normalDistFrom($E);

if ($distFromS->[$E->[0]][$E->[1]] == -1) {
    say 0;
    exit;
}

my $normalCost = $distFromS->[$E->[0]][$E->[1]];
my $possibleCheats = 0;

for my $startPos (@trackCells) {
    my $sd = $distFromS->[$startPos->[0]][$startPos->[1]];
    next if $sd == -1;
    for my $d1 (@dirs) {
        my $m1 = [$startPos->[0] + $d1->[0], $startPos->[1] + $d1->[1]];
        next if $m1->[0] < 0 || $m1->[0] >= $h || $m1->[1] < 0 || $m1->[1] >= $w;
        for my $d2 (@dirs) {
            my $m2 = [$m1->[0] + $d2->[0], $m1->[1] + $d2->[1]];
            next if $m2->[0] < 0 || $m2->[0] >= $h || $m2->[1] < 0 || $m2->[1] >= $w;
            next if $walls[$m2->[0]][$m2->[1]];
            my $ed = $distFromE->[$m2->[0]][$m2->[1]];
            next if $ed == -1;
            my $newCost = $sd + 2 + $ed;
            my $saving = $normalCost - $newCost;
            $possibleCheats++ if $saving >= 100;
        }
    }
}

say $possibleCheats;
