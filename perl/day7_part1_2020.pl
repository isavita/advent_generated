
use strict;
use warnings;

my %contains;
open(my $fh, "<", "input.txt") or die "Error opening file: $!";
while (my $line = <$fh>) {
    chomp $line;
    my @parts = split(" bags contain ", $line);
    my $container = $parts[0];
    if ($parts[1] eq "no other bags.") {
        next;
    }
    my @containedBags = split(", ", $parts[1]);
    foreach my $bag (@containedBags) {
        my $bagName = join(" ", (split(" ", $bag))[1, 2]);
        push @{$contains{$bagName}}, $container;
    }
}
close($fh);

my $count = countCanContain("shiny gold", \%contains);
print "$count\n";

sub countCanContain {
    my ($target, $contains) = @_;
    my %seen;
    my $dfs;
    $dfs = sub {
        my ($bag) = @_;
        foreach my $outer (@{$contains->{$bag}}) {
            if (!$seen{$outer}) {
                $seen{$outer} = 1;
                $dfs->($outer);
            }
        }
    };
    $dfs->($target);
    return scalar keys %seen;
}
