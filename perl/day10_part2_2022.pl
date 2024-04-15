use strict;
use warnings;

my @x = (1);
open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
while (my $line = <$fh>) {
    chomp $line;
    if ($line eq 'noop') {
        push @x, $x[-1];
    } else {
        my ($cmd, $n) = split ' ', $line;
        push @x, $x[-1];
        push @x, $x[-1] + $n;
    }
}
close $fh;

my %grid;
for my $i (0..$#x) {
    my $crtx = $i % 40;
    my $crty = int($i / 40);
    if (abs($crtx - $x[$i]) <= 1) {
        $grid{"$crtx,$crty"} = 1;
    } else {
        delete $grid{"$crtx,$crty"};
    }
}

for my $y (0..5) {
    for my $x (0..39) {
        if (exists $grid{"$x,$y"}) {
            print '#';
        } else {
            print '.';
        }
    }
    print "\n";
}

sub abs {
    my $x = shift;
    return $x < 0 ? -$x : $x;
}