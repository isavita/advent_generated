
use strict;
use warnings;

my @matrix;
open(my $fh, "<", "input.txt") or die "Error opening file: $!";
while (my $line = <$fh>) {
    chomp $line;
    push @matrix, [split //, $line];
}
close($fh);

my $sum = sumOfPartNumbers(\@matrix);
print "$sum\n";

sub sumOfPartNumbers {
    my ($matrix) = @_;
    my $sum = 0;
    my @visited;
    foreach my $row (@$matrix) {
        push @visited, [(0) x scalar(@$row)];
    }

    for my $y (0 .. $#matrix) {
        for my $x (0 .. $#{$matrix->[$y]}) {
            if (!$visited[$y][$x] && $matrix->[$y][$x] =~ /\d/) {
                my ($number, $length) = extractNumber($matrix, $x, $y);
                if (isAdjacentToSymbol($matrix, $x, $y, $length)) {
                    $sum += $number;
                }
                for my $i (0 .. $length-1) {
                    $visited[$y][$x+$i] = 1;
                }
            }
        }
    }
    return $sum;
}

sub extractNumber {
    my ($matrix, $x, $y) = @_;
    my $numberStr = "";
    while ($x < scalar(@{$matrix->[$y]}) && $matrix->[$y][$x] =~ /\d/) {
        $numberStr .= $matrix->[$y][$x];
        $x++;
    }
    my $number = int($numberStr);
    return ($number, length($numberStr));
}

sub isAdjacentToSymbol {
    my ($matrix, $x, $y, $length) = @_;
    for my $i (0 .. $length-1) {
        if (checkAdjacent($matrix, $x+$i, $y)) {
            return 1;
        }
    }
    return 0;
}

sub checkAdjacent {
    my ($matrix, $x, $y) = @_;
    for my $dy (-1, 0, 1) {
        for my $dx (-1, 0, 1) {
            my $adjX = $x + $dx;
            my $adjY = $y + $dy;
            if ($adjY >= 0 && $adjY < scalar(@$matrix) && $adjX >= 0 && $adjX < scalar(@{$matrix->[$adjY]})) {
                if (!($matrix->[$adjY][$adjX] =~ /\d/) && $matrix->[$adjY][$adjX] ne '.') {
                    return 1;
                }
            }
        }
    }
    return 0;
}
