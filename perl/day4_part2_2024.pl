
#!/usr/bin/perl
use strict;
use warnings;

sub check_mas {
    my ($grid, $x, $y, $dx, $dy) = @_;
    my $rows = @$grid;
    my $cols = length($grid->[0]);
    return 0 if $x < 0 || $y < 0 || $x >= $rows || $y >= $cols;

    my $word = "MAS";
    my ($forward, $backward) = (1, 1);

    for my $i (0..length($word)-1) {
        my ($new_x, $new_y) = ($x + $dx * $i, $y + $dy * $i);
        if ($new_x < 0 || $new_y < 0 || $new_x >= $rows || $new_y >= $cols || substr($grid->[$new_x], $new_y, 1) ne substr($word, $i, 1)) {
            $forward = 0;
            last;
        }
    }

    for my $i (0..length($word)-1) {
        my ($new_x, $new_y) = ($x + $dx * $i, $y + $dy * $i);
        if ($new_x < 0 || $new_y < 0 || $new_x >= $rows || $new_y >= $cols || substr($grid->[$new_x], $new_y, 1) ne substr($word, length($word)-1-$i, 1)) {
            $backward = 0;
            last;
        }
    }
    return $forward || $backward;
}

sub check_xmas {
    my ($grid, $x, $y) = @_;
    return check_mas($grid, $x-1, $y-1, 1, 1) && check_mas($grid, $x-1, $y+1, 1, -1) ||
           check_mas($grid, $x+1, $y-1, -1, 1) && check_mas($grid, $x+1, $y+1, -1, -1);
}

sub count_xmas_patterns {
    my ($grid) = @_;
    my $rows = @$grid;
    return 0 if $rows < 3 || length($grid->[0]) < 3;

    my $count = 0;
    for my $i (1..$rows-2) {
        for my $j (1..length($grid->[$i])-2) {
            $count++ if substr($grid->[$i], $j, 1) eq 'A' && check_xmas($grid, $i, $j);
        }
    }
    return $count;
}

open my $fh, "<", "input.txt" or die "Could not open file: $!";
my @grid = grep { length $_ } map { chomp; $_ } <$fh>;
close $fh;

my $count = count_xmas_patterns(\@grid);
print "X-MAS patterns appear $count times in the word search\n";
