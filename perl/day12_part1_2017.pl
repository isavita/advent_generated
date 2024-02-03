
use strict;
use warnings;

my %adj;
my %visited;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";

while (my $line = <$fh>) {
    chomp $line;
    my @parts = split / <-> /, $line;
    my $from = $parts[0];
    my @toNodes = split /, /, $parts[1];

    foreach my $toNode (@toNodes) {
        push @{$adj{$from}}, $toNode;
        push @{$adj{$toNode}}, $from;
    }
}

close $fh;

sub DFS {
    my $node = shift;
    $visited{$node} = 1;

    foreach my $neighbor (@{$adj{$node}}) {
        if (!$visited{$neighbor}) {
            DFS($neighbor);
        }
    }
}

DFS(0);

my $count = 0;
foreach my $v (values %visited) {
    if ($v) {
        $count++;
    }
}

print "$count\n";
