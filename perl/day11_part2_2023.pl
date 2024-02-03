
use strict;
use warnings;

my $empty = '.';

sub buildGrid {
    my @input = @_;
    my $width = length($input[0]);
    my $height = scalar @input;
    my %data;

    for my $y (0..$#input) {
        my @line = split('', $input[$y]);
        for my $x (0..$#line) {
            my $char = $line[$x];
            if ($char ne $empty) {
                $data{"$x,$y"} = $char;
            }
        }
    }

    return {
        width => $width,
        height => $height,
        data => \%data,
    };
}

sub toString {
    my ($grid, $empty) = @_;
    my $result = '';

    for my $y (0..$grid->{height}-1) {
        for my $x (0..$grid->{width}-1) {
            my $coord = "$x,$y";
            if (exists $grid->{data}{$coord}) {
                $result .= $grid->{data}{$coord};
            } else {
                $result .= $empty;
            }
        }
        $result .= "\n";
    }

    return $result;
}

sub getEmptyRows {
    my ($grid) = @_;
    my @emptyRows;

    for my $y (0..$grid->{height}-1) {
        my $isEmpty = 1;
        for my $x (0..$grid->{width}-1) {
            if (exists $grid->{data}{"$x,$y"}) {
                $isEmpty = 0;
                last;
            }
        }

        if ($isEmpty) {
            push @emptyRows, $y;
        }
    }

    return @emptyRows;
}

sub getEmptyCols {
    my ($grid) = @_;
    my @emptyCols;

    for my $x (0..$grid->{width}-1) {
        my $isEmpty = 1;
        for my $y (0..$grid->{height}-1) {
            if (exists $grid->{data}{"$x,$y"}) {
                $isEmpty = 0;
                last;
            }
        }

        if ($isEmpty) {
            push @emptyCols, $x;
        }
    }

    return @emptyCols;
}

sub calculateOffsets {
    my ($emptyIndexes, $bound) = @_;
    my @offsets = (0) x $bound;

    for my $idx (@$emptyIndexes) {
        for my $i ($idx+1..$bound-1) {
            $offsets[$i]++;
        }
    }

    return @offsets;
}

sub expandGrid {
    my ($grid, $expansionFactor) = @_;
    my @emptyCols = getEmptyCols($grid);
    my @emptyRows = getEmptyRows($grid);
    my $numLinesToAdd = $expansionFactor - 1;
    my %newGrid;

    my @dXs = calculateOffsets(\@emptyCols, $grid->{width});
    my @dYs = calculateOffsets(\@emptyRows, $grid->{height});

    for my $y (0..$grid->{height}-1) {
        for my $x (0..$grid->{width}-1) {
            my $coord = "$x,$y";
            if (exists $grid->{data}{$coord}) {
                my $newCoord = ($x + $dXs[$x] * $numLinesToAdd) . ',' . ($y + $dYs[$y] * $numLinesToAdd);
                $newGrid{$newCoord} = $grid->{data}{$coord};
            }
        }
    }

    return {
        width => $grid->{width} + scalar(@emptyCols) * $numLinesToAdd,
        height => $grid->{height} + scalar(@emptyRows) * $numLinesToAdd,
        data => \%newGrid,
    };
}

sub abs {
    my ($x) = @_;
    return $x < 0 ? -$x : $x;
}

sub calculateLength {
    my ($grid, $c1, $c2) = @_;
    my ($x1, $y1) = split(',', $c1);
    my ($x2, $y2) = split(',', $c2);
    my $dX = abs($x2 - $x1);
    my $dY = abs($y2 - $y1);
    return $dX + $dY;
}

sub solve {
    my ($input, $expansionFactor) = @_;
    my $grid = buildGrid(@$input);
    my $expandedGrid = expandGrid($grid, $expansionFactor);
    my $res = 0;
    my %alreadySeen;

    for my $coord1 (keys %{$expandedGrid->{data}}) {
        for my $coord2 (keys %alreadySeen) {
            my $length = calculateLength($expandedGrid, $coord1, $coord2);
            $res += $length;
        }
        $alreadySeen{$coord1} = 1;
    }

    return $res;
}

sub readFile {
    my ($fileName) = @_;
    open(my $fh, '<', $fileName) or die "Cannot open file $fileName: $!";
    my @lines = <$fh>;
    close($fh);
    chomp(@lines);
    return \@lines;
}

my $input = readFile("input.txt");
print solve($input, 1000000) . "\n";
