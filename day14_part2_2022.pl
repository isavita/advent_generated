
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my $input = do { local $/; <$fh> };
close $fh;

my @matrix = parseInput($input);
my $originCol = 0;
foreach my $i (0..$#{$matrix[0]}) {
    if ($matrix[0][$i] eq "+") {
        $originCol = $i;
    }
    $matrix[$#matrix][$i] = "#";
}

my $ans = 0;
while (!dropSand(\@matrix, $originCol)) {
    $ans++;
    if ($matrix[0][$originCol] eq "o") {
        last;
    }
}

print $ans;

sub parseInput {
    my ($input) = @_;
    my @coordSets;
    my $lowestCol = 999999;
    my $highestRow = 0;

    foreach my $line (split /\n/, $input) {
        my @rawCoords = split / -> /, $line;
        my @coords;
        foreach my $rawCoord (@rawCoords) {
            my ($col, $row) = split /,/, $rawCoord;
            push @coords, [$col, $row];

            $lowestCol = minInt($lowestCol, $col);
            $highestRow = maxInt($highestRow, $row);
        }
        push @coordSets, \@coords;
    }

    my $ExtraLeftSpace = 200;
    my $highestCol = 0;

    foreach my $set (@coordSets) {
        foreach my $coord (@$set) {
            $coord->[0] -= $lowestCol - $ExtraLeftSpace;
            $highestCol = maxInt($highestCol, $coord->[0]);
        }
    }

    my @matrix;
    foreach my $r (0..$highestRow+2) {
        $matrix[$r] = [((".") x ($highestCol + $ExtraLeftSpace * 2))];
    }

    foreach my $set (@coordSets) {
        foreach my $i (1..$#$set) {
            my @cols = ($set->[$i-1][0], $set->[$i][0]);
            my @rows = ($set->[$i-1][1], $set->[$i][1]);

            @cols = sort {$a <=> $b} @cols;
            @rows = sort {$a <=> $b} @rows;

            if ($cols[0] == $cols[1]) {
                foreach my $r ($rows[0]..$rows[1]) {
                    $matrix[$r][$cols[0]] = "#";
                }
            } elsif ($rows[0] == $rows[1]) {
                foreach my $c ($cols[0]..$cols[1]) {
                    $matrix[$rows[0]][$c] = "#";
                }
            }
        }
    }

    my $originCol = 500 - $lowestCol + $ExtraLeftSpace;
    $matrix[0][$originCol] = "+";

    foreach my $i (0..$#matrix) {
        foreach my $j (0..$#{$matrix[$i]}) {
            $matrix[$i][$j] = "." if $matrix[$i][$j] eq "";
        }
    }

    return @matrix;
}

sub dropSand {
    my ($matrix, $originCol) = @_;
    my ($r, $c) = (0, $originCol);

    while ($r < $#{$matrix}) {
        my $below = $matrix->[$r+1][$c];
        my $diagonallyLeft = $matrix->[$r+1][$c-1];
        my $diagonallyRight = $matrix->[$r+1][$c+1];
        if ($below eq ".") {
            $r++;
        } elsif ($diagonallyLeft eq ".") {
            $r++;
            $c--;
        } elsif ($diagonallyRight eq ".") {
            $r++;
            $c++;
        } else {
            $matrix->[$r][$c] = "o";
            return 0;
        }
    }

    return 1;
}

sub minInt {
    my ($a, $b) = @_;
    return $a < $b ? $a : $b;
}

sub maxInt {
    my ($a, $b) = @_;
    return $a > $b ? $a : $b;
}
