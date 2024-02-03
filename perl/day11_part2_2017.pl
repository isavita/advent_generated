
use strict;
use warnings;

sub abs {
    my $x = shift;
    if ($x < 0) {
        return -$x;
    } else {
        return $x;
    }
}

sub max {
    my ($a, $b) = @_;
    if ($a > $b) {
        return $a;
    } else {
        return $b;
    }
}

sub distance {
    my ($x, $y, $z) = @_;
    return (abs($x) + abs($y) + abs($z)) / 2;
}

open my $fh, '<', 'input.txt' or die "File reading error: $!";
my $input = <$fh>;
close $fh;

chomp $input;
my @directions = split(',', $input);

my ($x, $y, $z) = (0, 0, 0);
my $maxDistance = 0;

foreach my $dir (@directions) {
    if ($dir eq "n") {
        $y++;
        $z--;
    } elsif ($dir eq "ne") {
        $x++;
        $z--;
    } elsif ($dir eq "se") {
        $x++;
        $y--;
    } elsif ($dir eq "s") {
        $y--;
        $z++;
    } elsif ($dir eq "sw") {
        $x--;
        $z++;
    } elsif ($dir eq "nw") {
        $x--;
        $y++;
    }

    my $curDistance = distance($x, $y, $z);
    $maxDistance = max($maxDistance, $curDistance);
}

print "$maxDistance\n";
