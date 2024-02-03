
use strict;
use warnings;

sub readAll {
    my $file = shift;
    open(my $fh, '<', $file) or die "Could not open file '$file' $!";
    local $/ = undef;
    my $content = <$fh>;
    close($fh);
    return $content;
}

sub abs {
    my $n = shift;
    if ($n < 0) {
        return -$n;
    }
    return $n;
}

sub manhattan {
    my ($p, $q) = @_;
    return abs($p->{X} - $q->{X}) + abs($p->{Y} - $q->{Y});
}

sub impossible {
    my ($sensors, $y) = @_;
    my %pts;
    for my $s (@$sensors) {
        my $dist = $s->{dist} - abs($s->{pos}->{Y} - $y);
        for my $x (0..$dist) {
            $pts{$s->{pos}->{X} + $x} = 1;
            $pts{$s->{pos}->{X} - $x} = 1;
        }
    }
    for my $s (@$sensors) {
        if ($s->{beacon}->{Y} == $y) {
            delete $pts{$s->{beacon}->{X}};
        }
    }
    return scalar keys %pts;
}

my $input = readAll("input.txt");
my @sensors;
for my $line (split("\n", $input)) {
    my %s;
    if ($line =~ /Sensor at x=(\d+), y=(\d+): closest beacon is at x=(\d+), y=(\d+)/) {
        $s{pos}{X} = $1;
        $s{pos}{Y} = $2;
        $s{beacon}{X} = $3;
        $s{beacon}{Y} = $4;
        $s{dist} = manhattan(\%{ $s{pos} }, \%{ $s{beacon} });
        push @sensors, \%s;
    }
}
print impossible(\@sensors, 2000000) . "\n";
