use strict;
use warnings;
use List::Util qw( min );
use List::MoreUtils qw( pairwise );

sub dijkstra {
    my $grid = shift;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};

    my @pq;
    my %dist;
    $dist{"0,0"} = 0;
    push @pq, [0, 0, 0];

    my @directions = ([1, 0], [0, 1], [-1, 0], [0, -1]);

    while (@pq) {
        my $curr = shift @pq;
        my ($x, $y, $risk) = @$curr;

        if ($x == $rows - 1 && $y == $cols - 1) {
            return $risk;
        }

        foreach my $d (@directions) {
            my ($dx, $dy) = @$d;
            my $nx = $x + $dx;
            my $ny = $y + $dy;

            if ($nx >= 0 && $ny >= 0 && $nx < $rows && $ny < $cols) {
                my $next_risk = $risk + $grid->[$nx][$ny];
                my $key = "$nx,$ny";

                if (!exists $dist{$key} || $next_risk < $dist{$key}) {
                    $dist{$key} = $next_risk;
                    push @pq, [$nx, $ny, $next_risk];
                }
            }
        }
        @pq = sort { $a->[2] <=> $b->[2] } @pq;
    }
    return -1;
}

sub extend_grid {
    my $initial_grid = shift;
    my $rows = scalar @$initial_grid;
    my $cols = scalar @{$initial_grid->[0]};
    
    my @extended_grid;
    for my $i (0..$rows*5-1) {
        for my $j (0..$cols*5-1) {
            my $new_risk = $initial_grid->[$i % $rows][$j % $cols] + int($i / $rows) + int($j / $cols);
            $new_risk -= 9 if $new_risk > 9;
            $extended_grid[$i][$j] = $new_risk;
        }
    }
    return \@extended_grid;
}

open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my @initial_grid;
while (my $line = <$fh>) {
    chomp $line;
    my @row = map { $_ - '0' } split //, $line;
    push @initial_grid, \@row;
}
close $fh;

my $extended_grid = extend_grid(\@initial_grid);
print dijkstra($extended_grid) . "\n";