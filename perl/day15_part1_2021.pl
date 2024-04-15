use strict;
use warnings;
use List::Util qw(min);

sub dijkstra {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};
    my @dist;
    for my $i (0..$rows-1) {
        for my $j (0..$cols-1) {
            $dist[$i][$j] = 1e9;
        }
    }
    $dist[0][0] = 0;

    my @directions = ([1, 0], [0, 1], [-1, 0], [0, -1]);
    my @queue = ([0, 0, 0]);

    while (@queue) {
        my ($risk, $x, $y) = @{shift @queue};
        next if $risk > $dist[$x][$y];
        return $risk if $x == $rows-1 && $y == $cols-1;
        for my $d (@directions) {
            my $nx = $x + $d->[0];
            my $ny = $y + $d->[1];
            if ($nx >= 0 && $ny >= 0 && $nx < $rows && $ny < $cols) {
                my $nextRisk = $risk + $grid->[$nx][$ny];
                if ($nextRisk < $dist[$nx][$ny]) {
                    $dist[$nx][$ny] = $nextRisk;
                    push @queue, [$nextRisk, $nx, $ny];
                    @queue = sort { $a->[0] <=> $b->[0] } @queue;
                }
            }
        }
    }
    return -1;
}

open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my @grid;
while (my $line = <$fh>) {
    chomp $line;
    my @row = split //, $line;
    push @grid, \@row;
}
close $fh;

my $result = dijkstra(\@grid);
print "$result\n";