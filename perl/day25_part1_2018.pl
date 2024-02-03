
use strict;
use warnings;

sub abs {
    my $x = shift;
    return $x < 0 ? -$x : $x;
}

sub manhattanDistance {
    my ($a, $b) = @_;
    return abs($a->{x} - $b->{x}) + abs($a->{y} - $b->{y}) + abs($a->{z} - $b->{z}) + abs($a->{t} - $b->{t});
}

package UnionFind;

sub new {
    my ($class, $size) = @_;
    my $parent = [0..$size-1];
    return bless { parent => $parent }, $class;
}

sub find {
    my ($self, $x) = @_;
    if ($self->{parent}[$x] != $x) {
        $self->{parent}[$x] = $self->find($self->{parent}[$x]);
    }
    return $self->{parent}[$x];
}

sub union {
    my ($self, $x, $y) = @_;
    my $rootX = $self->find($x);
    my $rootY = $self->find($y);
    if ($rootX != $rootY) {
        $self->{parent}[$rootX] = $rootY;
    }
}

package main;

open(my $fh, '<', 'input.txt') or die $!;
my @points;
while (my $line = <$fh>) {
    chomp $line;
    my @coords = split(',', $line);
    my ($x, $y, $z, $t) = map { int($_) } @coords;
    push @points, { x => $x, y => $y, z => $z, t => $t };
}
close($fh);

my $uf = UnionFind->new(scalar @points);
for my $i (0..$#points) {
    for my $j (0..$#points) {
        if (manhattanDistance($points[$i], $points[$j]) <= 3) {
            $uf->union($i, $j);
        }
    }
}

my $constellationCount = 0;
for my $i (0..$#{$uf->{parent}}) {
    if ($i == $uf->find($i)) {
        $constellationCount++;
    }
}
print "$constellationCount\n";
