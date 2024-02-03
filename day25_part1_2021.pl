
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @lines = <$fh>;
close($fh);

my @grid;
foreach my $line (@lines) {
    chomp($line);
    push @grid, [split //, $line];
}

my $step = 0;
while (1) {
    my $eastMoved = moveEast(\@grid);
    my $southMoved = moveSouth(\@grid);
    $step++;

    if (!$eastMoved && !$southMoved) {
        last;
    }
}

sub moveEast {
    my ($grid) = @_;
    my $moved = 0;
    my $height = scalar(@$grid);
    my $width = scalar(@{$grid->[0]});
    my @oldPositions;

    for (my $i = 0; $i < $height; $i++) {
        push @oldPositions, [ (0) x $width ];
    }

    for (my $y = 0; $y < $height; $y++) {
        for (my $x = 0; $x < $width; $x++) {
            if ($grid->[$y][$x] eq '>') {
                my $nextX = ($x + 1) % $width;
                if ($grid->[$y][$nextX] eq '.') {
                    $oldPositions[$y][$x] = '.';
                    $grid->[$y][$nextX] = '>';
                    $x++;
                    $moved = 1;
                }
            }
        }
    }

    freeEmptyPositions($grid, \@oldPositions);
    return $moved;
}

sub moveSouth {
    my ($grid) = @_;
    my $moved = 0;
    my $height = scalar(@$grid);
    my $width = scalar(@{$grid->[0]});
    my @oldPositions;

    for (my $i = 0; $i < $height; $i++) {
        push @oldPositions, [ (0) x $width ];
    }

    for (my $x = 0; $x < $width; $x++) {
        for (my $y = 0; $y < $height; $y++) {
            if ($grid->[$y][$x] eq 'v') {
                my $nextY = ($y + 1) % $height;
                if ($grid->[$nextY][$x] eq '.') {
                    $oldPositions[$y][$x] = '.';
                    $grid->[$nextY][$x] = 'v';
                    $y++;
                    $moved = 1;
                }
            }
        }
    }

    freeEmptyPositions($grid, \@oldPositions);
    return $moved;
}

sub freeEmptyPositions {
    my ($grid, $oldPositions) = @_;
    my $height = scalar(@$grid);
    my $width = scalar(@{$grid->[0]});

    for (my $y = 0; $y < $height; $y++) {
        for (my $x = 0; $x < $width; $x++) {
            if ($oldPositions->[$y][$x] eq '.') {
                $grid->[$y][$x] = '.';
            }
        }
    }
}

print "$step\n";
