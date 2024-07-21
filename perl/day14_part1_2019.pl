
use strict;
use warnings;

my %reactions;
my %ingredients;

open my $fh, '<', 'input.txt' or die "Error opening file: $!";
while (<$fh>) {
    chomp;
    my ($inputs, $output) = split ' => ';
    my ($amount, $name) = split ' ', $output;
    $reactions{$name} = $amount;
    $ingredients{$name} = [ map { [ split ' ', $_ ] } split ', ', $inputs ];
}
close $fh;

sub calculate_ore {
    my ($chem, $amount, $surplus) = @_;
    return $amount if $chem eq 'ORE';

    if ($surplus->{$chem} >= $amount) {
        $surplus->{$chem} -= $amount;
        return 0;
    }

    $amount -= $surplus->{$chem};
    $surplus->{$chem} = 0;
    my $reaction_amount = $reactions{$chem};
    my $times = int(($amount + $reaction_amount - 1) / $reaction_amount);
    my $ore = 0;

    for my $ingredient (@{ $ingredients{$chem} }) {
        $ore += calculate_ore($ingredient->[1], $ingredient->[0] * $times, $surplus);
    }

    $surplus->{$chem} += $times * $reaction_amount - $amount;
    return $ore;
}

my $total_ore = calculate_ore('FUEL', 1, {});
print "$total_ore\n";
