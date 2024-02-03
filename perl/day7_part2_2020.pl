
use strict;
use warnings;

my %rules;
my $ruleRegex = qr/(\d+) (\w+ \w+) bags?[,.]/;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
while (my $line = <$fh>) {
    chomp $line;
    my ($container, $contents) = split / bags contain /, $line;

    next if $contents eq "no other bags.";

    while ($contents =~ /$ruleRegex/g) {
        my $count = $1;
        my $color = $2;
        push @{$rules{$container}}, {Color => $color, Count => $count};
    }
}
close($fh);

my $totalBags = countBags("shiny gold", \%rules) - 1;
print "$totalBags\n";

sub countBags {
    my ($color, $rules) = @_;
    my $count = 1;
    foreach my $rule (@{$rules->{$color}}) {
        $count += $rule->{Count} * countBags($rule->{Color}, $rules);
    }
    return $count;
}
