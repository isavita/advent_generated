
use strict;
use warnings;

my @nodes = readNodes("input.txt");
my $viablePairs = countViablePairs(\@nodes);
print "$viablePairs\n";

sub readNodes {
    my ($filename) = @_;
    open my $fh, '<', $filename or die $!;
    
    my @nodes;
    my $nodeRegex = qr/node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%/;
    while (my $line = <$fh>) {
        if (my @matches = $line =~ $nodeRegex) {
            my ($used, $avail) = @matches;
            push @nodes, {used => $used, avail => $avail};
        }
    }
    close $fh;
    return @nodes;
}

sub countViablePairs {
    my ($nodes) = @_;
    my $count = 0;
    for my $i (0..$#{$nodes}) {
        for my $j (0..$#{$nodes}) {
            my $a = $nodes->[$i];
            my $b = $nodes->[$j];
            if ($i != $j && $a->{used} > 0 && $a->{used} <= $b->{avail}) {
                $count++;
            } else {
                next;
            }
        }
    }
    return $count;
}
