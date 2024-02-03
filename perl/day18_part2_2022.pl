
use strict;
use warnings;

my %cubes;
my @neighbors = (
    [-1, 0, 0],
    [1, 0, 0],
    [0, -1, 0],
    [0, 1, 0],
    [0, 0, -1],
    [0, 0, 1]
);
my $min = { X => 999999, Y => 999999, Z => 999999 };
my $max = { X => -999999, Y => -999999, Z => -999999 };

open(my $fh, '<', 'input.txt') or die $!;

while (my $line = <$fh>) {
    chomp $line;
    next if $line eq "";

    my @values = split(',', $line);
    my $cube = { X => $values[0], Y => $values[1], Z => $values[2] };
    $cubes{"$cube->{X},$cube->{Y},$cube->{Z}"} = 1;
    $min->{X} = min($min->{X}, $cube->{X});
    $min->{Y} = min($min->{Y}, $cube->{Y});
    $min->{Z} = min($min->{Z}, $cube->{Z});
    $max->{X} = max($max->{X}, $cube->{X});
    $max->{Y} = max($max->{Y}, $cube->{Y});
    $max->{Z} = max($max->{Z}, $cube->{Z});
}

$min->{X} -= 1;
$min->{Y} -= 1;
$min->{Z} -= 1;
$max->{X} += 1;
$max->{Y} += 1;
$max->{Z} += 1;

my $faces = 0;
my @q = ($min);
my %seen = ($min => 1);

while (@q) {
    my $curr = shift @q;
    foreach my $delta (@neighbors) {
        my $next = { X => $curr->{X} + $delta->[0], Y => $curr->{Y} + $delta->[1], Z => $curr->{Z} + $delta->[2] };
        next if $next->{X} < $min->{X} || $next->{Y} < $min->{Y} || $next->{Z} < $min->{Z} || $next->{X} > $max->{X} || $next->{Y} > $max->{Y} || $next->{Z} > $max->{Z};
        if (exists $cubes{"$next->{X},$next->{Y},$next->{Z}"}) {
            $faces++;
        } elsif (!exists $seen{"$next->{X},$next->{Y},$next->{Z}"}) {
            $seen{"$next->{X},$next->{Y},$next->{Z}"} = 1;
            push @q, $next;
        }
    }
}

print "$faces\n";

sub min {
    my ($a, $b) = @_;
    return $a < $b ? $a : $b;
}

sub max {
    my ($a, $b) = @_;
    return $a > $b ? $a : $b;
}
