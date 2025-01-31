
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my %graph;

while (my $line = <$fh>) {
    chomp $line;
    my ($c1, $c2) = split('-', $line);
    next unless defined $c2;
    $graph{$c1}->{$c2} = 1;
    $graph{$c2}->{$c1} = 1;
}

close($fh);

my @computers = keys %graph;
my %seen;
my $count = 0;

for my $i (0 .. $#computers - 2) {
    for my $j ($i + 1 .. $#computers - 1) {
        for my $k ($j + 1 .. $#computers) {
            my ($c1, $c2, $c3) = ($computers[$i], $computers[$j], $computers[$k]);
            if ($graph{$c1}->{$c2} && $graph{$c2}->{$c3} && $graph{$c1}->{$c3}) {
                if ($c1 =~ /^t/ || $c2 =~ /^t/ || $c3 =~ /^t/) {
                    my $key = join(',', sort { $a cmp $b } ($c1, $c2, $c3));
                    $count++ unless $seen{$key}++;
                }
            }
        }
    }
}

print "Number of triplets containing at least one computer with name starting with 't': $count\n";
