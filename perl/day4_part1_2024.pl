
#!/usr/bin/perl

use strict;
use warnings;

my @directions = (
    [0, 1],   # right
    [1, 0],   # down
    [1, 1],   # diagonal down-right
    [-1, 1],  # diagonal up-right
    [0, -1],  # left
    [-1, 0],  # up
    [-1, -1], # diagonal up-left
    [1, -1],  # diagonal down-left
);

sub checkWord {
    my ($grid, $word, $x, $y, $dx, $dy) = @_;
    return 0 unless $x >= 0 && $y >= 0 && $x < @$grid && $y < length $grid->[0];

    for my $i (0..length($word)-1) {
        my $nx = $x + $dx * $i;
        my $ny = $y + $dy * $i;
        return 0 unless $nx >= 0 && $ny >= 0 && $nx < @$grid && $ny < length $grid->[0];
        return 0 unless substr($grid->[$nx], $ny, 1) eq substr($word, $i, 1);
    }
    return 1;
}

sub countOccurrences {
    my ($grid, $word) = @_;
    my $count = 0;
    for my $x (0..$#$grid) {
        for my $y (0..length($grid->[0])-1) {
            for my $dir (@directions) {
                $count++ if checkWord($grid, $word, $x, $y, $dir->[0], $dir->[1]);
            }
        }
    }
    return $count;
}


open(my $fh, "<", "input.txt") or die "Could not open file: $!";

my @grid;
while (my $line = <$fh>) {
    chomp $line;
    push @grid, $line unless $line eq "";
}
close $fh;


my $count = countOccurrences(\@grid, "XMAS");
print "XMAS appears $count times in the word search\n";
