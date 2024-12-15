
#!/usr/bin/perl
use strict;
use warnings;

sub solve {
    my @grid = @_;
    my $rows = @grid;
    return 0 if $rows == 0;
    my $cols = length $grid[0];

    my @visited;
    for my $i (0..$rows-1) {
        $visited[$i] = [ (0) x $cols ];
    }

    my $total_price = 0;
    for my $r (0..$rows-1) {
        for my $c (0..$cols-1) {
            if (!$visited[$r][$c]) {
                my ($area, $perimeter) = calculate_region(\@grid, $r, $c, \@visited);
                $total_price += $area * $perimeter;
            }
        }
    }
    return $total_price;
}

sub calculate_region {
    my ($grid, $row, $col, $visited) = @_;
    my $rows = @$grid;
    my $cols = length $grid->[0];
    my $char = substr($grid->[$row], $col, 1);
    my $area = 0;
    my $perimeter = 0;

    my @queue = ([$row, $col]);
    $visited->[$row][$col] = 1;

    while (@queue) {
        my $p = shift @queue;
        my ($x, $y) = @$p;

        $area++;

        my $is_border = ($x == 0 || $x == $rows - 1 || $y == 0 || $y == $cols - 1);

        # Check top
        if ($x > 0) {
            if (substr($grid->[$x - 1], $y, 1) ne $char) {
                $perimeter++;
            } elsif (!$visited->[$x - 1][$y]) {
                push @queue, [$x - 1, $y];
                $visited->[$x - 1][$y] = 1;
            }
        } elsif ($is_border) {
            $perimeter++;
        }
        # Check bottom
        if ($x < $rows - 1) {
            if (substr($grid->[$x + 1], $y, 1) ne $char) {
                $perimeter++;
            } elsif (!$visited->[$x + 1][$y]) {
                push @queue, [$x + 1, $y];
                $visited->[$x + 1][$y] = 1;
            }
        } elsif ($is_border) {
            $perimeter++;
        }
        # Check left
        if ($y > 0) {
            if (substr($grid->[$x], $y - 1, 1) ne $char) {
                $perimeter++;
            } elsif (!$visited->[$x][$y - 1]) {
                push @queue, [$x, $y - 1];
                $visited->[$x][$y - 1] = 1;
            }
        } elsif ($is_border) {
            $perimeter++;
        }
        # Check right
        if ($y < $cols - 1) {
            if (substr($grid->[$x], $y + 1, 1) ne $char) {
                $perimeter++;
            } elsif (!$visited->[$x][$y + 1]) {
                push @queue, [$x, $y + 1];
                $visited->[$x][$y + 1] = 1;
            }
        } elsif ($is_border) {
            $perimeter++;
        }
    }
    return ($area, $perimeter);
}

open my $fh, "<", "input.txt" or die "Could not open file: $!";
my @grid;
while (my $line = <$fh>) {
    chomp $line;
    push @grid, $line;
}
close $fh;

my $total_price = solve(@grid);
print "$total_price\n";
