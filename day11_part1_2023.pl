
use strict;
use warnings;

my $empty = '.';

sub buildGrid {
    my @input = @_;
    my %grid = ();
    my $width = length($input[0]);
    my $height = scalar @input;

    for my $y (0..$#input) {
        my @line = split('', $input[$y]);
        for my $x (0..$#line) {
            my $char = $line[$x];
            if ($char ne $empty) {
                $grid{"$x,$y"} = $char;
            }
        }
    }

    return (\%grid, $width, $height);
}

sub toString {
    my ($gridRef, $width, $height) = @_;
    my %grid = %$gridRef;
    my $result = '';

    for my $y (0..$height-1) {
        for my $x (0..$width-1) {
            if (exists $grid{"$x,$y"}) {
                $result .= $grid{"$x,$y"};
            } else {
                $result .= $empty;
            }
        }
        $result .= "\n";
    }

    return $result;
}

sub getEmptyRows {
    my ($gridRef, $width, $height) = @_;
    my %grid = %$gridRef;
    my @emptyRows = ();

    for my $y (0..$height-1) {
        my $isEmpty = 1;

        for my $x (0..$width-1) {
            if (exists $grid{"$x,$y"}) {
                $isEmpty = 0;
            }
        }

        if ($isEmpty) {
            push @emptyRows, $y;
        }
    }

    return @emptyRows;
}

sub getEmptyCols {
    my ($gridRef, $width, $height) = @_;
    my %grid = %$gridRef;
    my @emptyCols = ();

    for my $x (0..$width-1) {
        my $isEmpty = 1;

        for my $y (0..$height-1) {
            if (exists $grid{"$x,$y"}) {
                $isEmpty = 0;
            }
        }

        if ($isEmpty) {
            push @emptyCols, $x;
        }
    }

    return @emptyCols;
}

sub calculateOffsets {
    my ($emptyIndexesRef, $bound) = @_;
    my @emptyIndexes = @$emptyIndexesRef;
    my @offsets = (0) x $bound;

    for my $idx (@emptyIndexes) {
        for my $i ($idx+1..$bound-1) {
            $offsets[$i]++;
        }
    }

    return @offsets;
}

sub expandGrid {
    my ($gridRef, $width, $height, $expansionFactor) = @_;
    my %grid = %$gridRef;
    my @emptyCols = getEmptyCols(\%grid, $width, $height);
    my @emptyRows = getEmptyRows(\%grid, $width, $height);
    my $numLinesToAdd = $expansionFactor - 1;
    my %newGrid = ();

    my @dXs = calculateOffsets(\@emptyCols, $width);
    my @dYs = calculateOffsets(\@emptyRows, $height);

    for my $y (0..$height-1) {
        for my $x (0..$width-1) {
            if (exists $grid{"$x,$y"}) {
                my $newCoordX = $x + $dXs[$x] * $numLinesToAdd;
                my $newCoordY = $y + $dYs[$y] * $numLinesToAdd;
                $newGrid{"$newCoordX,$newCoordY"} = $grid{"$x,$y"};
            }
        }
    }

    return (\%newGrid, $width + scalar(@emptyCols) * $numLinesToAdd, $height + scalar(@emptyRows) * $numLinesToAdd);
}

sub abs {
    my $x = shift;
    if ($x < 0) {
        return -$x;
    }
    return $x;
}

sub calculateLength {
    my ($gridRef, $c1Ref, $c2Ref) = @_;
    my %grid = %$gridRef;
    my ($c1X, $c1Y) = split(',', $$c1Ref);
    my ($c2X, $c2Y) = split(',', $$c2Ref);
    my $dX = abs($c2X - $c1X);
    my $dY = abs($c2Y - $c1Y);
    return $dX + $dY;
}

sub solve {
    my @input = @_;
    my ($gridRef, $width, $height) = buildGrid(@input, $empty);
    my %grid = %$gridRef;
    my ($newGridRef, $newWidth, $newHeight) = expandGrid($gridRef, $width, $height, 2);
    my %newGrid = %$newGridRef;
    my $res = 0;
    my %alreadySeen = ();

    for my $coord1 (keys %newGrid) {
        for my $coord2 (keys %alreadySeen) {
            $res += calculateLength(\%newGrid, \$coord1, \$coord2);
        }
        $alreadySeen{$coord1} = 1;
    }

    return $res;
}

sub readFile {
    my $fileName = shift;
    open my $fh, '<', $fileName or die $!;
    my @lines = <$fh>;
    close $fh;
    chomp @lines;
    return @lines;
}

my @input = readFile("input.txt");
print solve(@input) . "\n";
