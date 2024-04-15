use strict;
use warnings;
use List::Util qw(min max);

my @rope = map { [0, 0] } (1..10);
my %visited;
$visited{"$rope[9][0],$rope[9][1]"} = 1;

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
while (my $line = <$fh>) {
    chomp $line;
    my ($dir, $steps) = split ' ', $line;
    for (1..$steps) {
        move_head($dir);
        for my $i (1..9) {
            $rope[$i] = next_knot($rope[$i-1], $rope[$i]);
        }
        $visited{"$rope[9][0],$rope[9][1]"} = 1;
    }
}
close $fh;

print scalar keys %visited, "\n";

sub move_head {
    my ($dir) = @_;
    if    ($dir eq 'U') { $rope[0][1]++ }
    elsif ($dir eq 'R') { $rope[0][0]++ }
    elsif ($dir eq 'D') { $rope[0][1]-- }
    elsif ($dir eq 'L') { $rope[0][0]-- }
}

sub next_knot {
    my ($head, $tail) = @_;
    my ($dx, $dy) = ($head->[0] - $tail->[0], $head->[1] - $tail->[1]);
    if (abs($dx) <= 1 && abs($dy) <= 1) {
        return $tail;
    }
    return [$tail->[0] + sign($dx), $tail->[1] + sign($dy)];
}

sub abs {
    return $_[0] < 0 ? -$_[0] : $_[0];
}

sub sign {
    return $_[0] <=> 0;
}