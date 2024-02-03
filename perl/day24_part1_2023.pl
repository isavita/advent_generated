
use strict;
use warnings;

sub parseInput {
    my @input = @_;
    my @points;
    foreach my $line (@input) {
        my @values = split /, | @ /, $line;
        my %point = (
            pos => { x => $values[0], y => $values[1], z => $values[2] },
            vel => { x => $values[3], y => $values[4], z => $values[5] }
        );
        push @points, \%point;
    }
    return @points;
}

sub isIntersecting2D {
    my ($p1, $p2) = @_;
    my $det = $p1->{vel}->{x} * $p2->{vel}->{y} - $p2->{vel}->{x} * $p1->{vel}->{y};
    if ($det == 0) {
        return (0, {}, 0, 0);
    }
    my $t1 = ($p2->{vel}->{y} * ($p2->{pos}->{x} - $p1->{pos}->{x}) - $p2->{vel}->{x} * ($p2->{pos}->{y} - $p1->{pos}->{y})) / $det;
    my $t2 = ($p1->{vel}->{y} * ($p2->{pos}->{x} - $p1->{pos}->{x}) - $p1->{vel}->{x} * ($p2->{pos}->{y} - $p1->{pos}->{y})) / $det;
    my %coord = (
        x => $p1->{pos}->{x} + $p1->{vel}->{x} * $t1,
        y => $p1->{pos}->{y} + $p1->{vel}->{y} * $t1,
        z => 0
    );
    return (1, \%coord, $t1, $t2);
}

sub solve {
    my ($min, $max, @input) = @_;
    my @points = parseInput(@input);

    my $cnt = 0;
    for (my $i = 0; $i < scalar @points; $i++) {
        for (my $j = 0; $j < $i; $j++) {
            my ($isIntersecting, $coord, $time1, $time2) = isIntersecting2D($points[$i], $points[$j]);
            my $isInBound = $min <= $coord->{x} && $coord->{x} <= $max && $min <= $coord->{y} && $coord->{y} <= $max;
            if ($isIntersecting && $isInBound && $time1 >= 0 && $time2 >= 0) {
                $cnt++;
            }
        }
    }
    return $cnt;
}

sub readFile {
    my ($fileName) = @_;
    open my $fh, '<', $fileName or die "Cannot open file: $!";
    my @lines = <$fh>;
    close $fh;
    chomp @lines;
    return @lines;
}

my @input = readFile("input.txt");
print solve(200000000000000, 400000000000000, @input);
