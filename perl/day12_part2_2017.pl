
use strict;
use warnings;

my %adj;
my %visited;
my $groups = 0;

open(my $fh, '<', 'input.txt') or die "File reading error $!";

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

foreach my $node (keys %adj) {
    if (!$visited{$node}) {
        DFS($node);
        $groups++;
    }
}

print "$groups\n";
