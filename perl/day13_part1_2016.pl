
use strict;
use warnings;

my $favoriteNumber;
open(my $fh, '<', 'input.txt') or die $!;
while (my $line = <$fh>) {
    $favoriteNumber = int($line);
}
close($fh);

my %visited;
my @queue = ({x => 1, y => 1});
my $steps = 0;

sub isWall {
    my ($favoriteNumber, $x, $y) = @_;
    my $num = $x*$x + 3*$x + 2*$x*$y + $y + $y*$y + $favoriteNumber;
    my $bits = 0;
    while ($num > 0) {
        if ($num % 2 == 1) {
            $bits++;
        }
        $num = int($num / 2);
    }
    return $bits % 2 != 0;
}

sub bfs {
    my ($start, $target, $favoriteNumber) = @_;
    my $steps = 0;

    while (@queue) {
        my $size = scalar @queue;
        for (my $i = 0; $i < $size; $i++) {
            my $point = shift @queue;
            if ($point->{x} == $target->{x} && $point->{y} == $target->{y}) {
                return $steps;
            }

            my @deltas = ({x => 1, y => 0}, {x => -1, y => 0}, {x => 0, y => 1}, {x => 0, y => -1});
            foreach my $delta (@deltas) {
                my $next = {x => $point->{x} + $delta->{x}, y => $point->{y} + $delta->{y}};
                if ($next->{x} >= 0 && $next->{y} >= 0 && !isWall($favoriteNumber, $next->{x}, $next->{y}) && !$visited{"$next->{x},$next->{y}"}) {
                    $visited{"$next->{x},$next->{y}"} = 1;
                    push @queue, $next;
                }
            }
        }
        $steps++;
    }

    return -1;
}

my $start = {x => 1, y => 1};
my $target = {x => 31, y => 39};
my $result = bfs($start, $target, $favoriteNumber);
print "$result\n";
