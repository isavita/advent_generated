
use strict;
use warnings;
use List::Util 'min';

open my $fh, '<', 'input.txt' or die $!;
my @grid = <$fh>;
close $fh;

my ($h, $w) = (scalar @grid, length $grid[0]);
my ($S, $E);
my @walls = map { [ (0) x $w ] } 0..$h-1;
my @trackCells;

for my $i (0..$h-1) {
    for my $j (0..$w-1) {
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

sub isTrack {
    my ($x, $y) = @_;
    return $x >= 0 && $x < $h && $y >= 0 && $y < $w && !$walls[$x][$y];
}

sub normalDistFrom {
    my ($start) = @_;
    my @dist = map { [( -1) x $w ] } 0..$h-1;
    $dist[$start->[0]][$start->[1]] = 0;
    my @q = ($start);

    while (@q) {
        my $cur = shift @q;
        for my $d (@dirs) {
            my ($nx, $ny) = ($cur->[0] + $d->[0], $cur->[1] + $d->[1]);
            if ($nx >= 0 && $nx < $h && $ny >= 0 && $ny < $w && !$walls[$nx][$ny] && $dist[$nx][$ny] < 0) {
                $dist[$nx][$ny] = $dist[$cur->[0]][$cur->[1]] + 1;
                push @q, [$nx, $ny];
            }
        }
    }
    return \@dist;
}

my $distFromS = normalDistFrom($S);
my $distFromE = normalDistFrom($E);
my $normalCost = $distFromS->[$E->[0]][$E->[1]];
if ($normalCost < 0) {
    print 0;
    exit;
}

my %cheats;
for my $startPos (@trackCells) {
    my $sd = $distFromS->[$startPos->[0]][$startPos->[1]];
    next if $sd < 0;

    my @distC = map { [( -1) x $w ] } 0..$h-1;
    $distC[$startPos->[0]][$startPos->[1]] = 0;
    my @q = ($startPos);

    while (@q) {
        my $cur = shift @q;
        my $steps = $distC[$cur->[0]][$cur->[1]];
        next if $steps == 20;
        for my $d (@dirs) {
            my ($nx, $ny) = ($cur->[0] + $d->[0], $cur->[1] + $d->[1]);
            if ($nx >= 0 && $nx < $h && $ny >= 0 && $ny < $w && $distC[$nx][$ny] < 0) {
                $distC[$nx][$ny] = $steps + 1;
                push @q, [$nx, $ny];
            }
        }
    }

    for my $x (0..$h-1) {
        for my $y (0..$w-1) {
            my $s = $distC[$x][$y];
            if ($s > 0 && $s <= 20 && isTrack($x, $y)) {
                my $ed = $distFromE->[$x][$y];
                if ($ed < 0) {
                    next;
                }
                my $cost = $sd + $s + $ed;
                if ($cost < $normalCost) {
                    my $key = "$startPos->[0],$startPos->[1],$x,$y";
                    $cheats{$key} = min($cheats{$key} // $cost, $cost);
                }
            }
        }
    }
}

my $count = 0;
for my $cost (values %cheats) {
    $count++ if ($normalCost - $cost >= 100);
}
print $count;
exit;
