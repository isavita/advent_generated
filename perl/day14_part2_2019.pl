
use strict;
use warnings;

sub parse_chemical {
    my $s = shift;
    my ($amount, $name) = split ' ', $s;
    return ($name, $amount);
}

sub calculate_ore {
    my ($chem, $amount, $reactions, $ingredients, $surplus) = @_;
    return $amount if $chem eq 'ORE';

    if ($surplus->{$chem} >= $amount) {
        $surplus->{$chem} -= $amount;
        return 0;
    }

    $amount -= $surplus->{$chem};
    $surplus->{$chem} = 0;
    my ($reaction_amount) = $reactions->{$chem};
    my $times = int(($amount + $reaction_amount - 1) / $reaction_amount);
    my $ore = 0;

    for my $ingredient (@{$ingredients->{$chem}}) {
        $ore += calculate_ore($ingredient->[0], $ingredient->[1] * $times, $reactions, $ingredients, $surplus);
    }

    $surplus->{$chem} += $times * $reaction_amount - $amount;
    return $ore;
}

sub max_fuel {
    my ($reactions, $ingredients, $ore_available) = @_;
    my ($low, $high) = (0, $ore_available);
    while ($low < $high) {
        my $mid = int(($low + $high + 1) / 2);
        if (calculate_ore('FUEL', $mid, $reactions, $ingredients, {}) > $ore_available) {
            $high = $mid - 1;
        } else {
            $low = $mid;
        }
    }
    return $low;
}

my %reactions;
my %ingredients;

open my $fh, '<', 'input.txt' or die "Error opening file: $!";
while (<$fh>) {
    chomp;
    my ($inputs, $output) = split ' => ';
    my ($output_name, $output_amount) = parse_chemical($output);
    my @input_list = map { [parse_chemical($_)] } split ', ', $inputs;
    $reactions{$output_name} = $output_amount;
    $ingredients{$output_name} = \@input_list;
}
close $fh;

my $ore_available = 1_000_000_000_000;
print max_fuel(\%reactions, \%ingredients, $ore_available), "\n";
