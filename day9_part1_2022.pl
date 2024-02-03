
use strict;
use warnings;

my $file = 'input.txt';
open(my $fh, '<', $file) or die "Could not open file '$file' $!";

my %visited = ();
my @head = (0, 0);
my @tail = (0, 0);
$visited{"0,0"} = 1;

while (my $line = <$fh>) {
    chomp $line;
    my @line = split(' ', $line);
    my $dir = $line[0];
    my $steps = $line[1];
    my $numSteps = int($steps);

    for (my $i = 0; $i < $numSteps; $i++) {
        if ($dir eq "R") {
            $head[0]++;
        } elsif ($dir eq "L") {
            $head[0]--;
        } elsif ($dir eq "U") {
            $head[1]++;
        } elsif ($dir eq "D") {
            $head[1]--;
        }

        if (abs($head[0]-$tail[0]) > 1 || abs($head[1]-$tail[1]) > 1) {
            if ($head[0] != $tail[0] && $head[1] != $tail[1]) {
                if ($head[0] > $tail[0]) {
                    $tail[0]++;
                } else {
                    $tail[0]--;
                }
                if ($head[1] > $tail[1]) {
                    $tail[1]++;
                } else {
                    $tail[1]--;
                }
            } else {
                if ($head[0] > $tail[0]) {
                    $tail[0]++;
                } elsif ($head[0] < $tail[0]) {
                    $tail[0]--;
                }
                if ($head[1] > $tail[1]) {
                    $tail[1]++;
                } elsif ($head[1] < $tail[1]) {
                    $tail[1]--;
                }
            }
        }

        $visited{"$tail[0],$tail[1]"} = 1;
    }
}

close($fh);

my $count = keys %visited;
print "$count\n";

sub abs {
    my $x = shift;
    if ($x < 0) {
        return -$x;
    }
    return $x;
}
