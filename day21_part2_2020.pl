
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my %allergenMap;
my %ingredientAllergen;

while (my $line = <$fh>) {
    chomp $line;
    my @parts = split / \(contains /, $line;
    my @ingredients = split ' ', $parts[0];
    my @allergens = ();
    if (@parts > 1) {
        @allergens = split ', ', substr($parts[1], 0, -1);
    }

    foreach my $allergen (@allergens) {
        if (!exists $allergenMap{$allergen}) {
            $allergenMap{$allergen} = { map { $_ => 1 } @ingredients };
        } else {
            foreach my $ingredient (keys %{$allergenMap{$allergen}}) {
                delete $allergenMap{$allergen}{$ingredient} unless grep { $_ eq $ingredient } @ingredients;
            }
        }
    }
}

while (keys %allergenMap) {
    foreach my $allergen (keys %allergenMap) {
        if (keys %{$allergenMap{$allergen}} == 1) {
            my $ingredient = (keys %{$allergenMap{$allergen}})[0];
            $ingredientAllergen{$allergen} = $ingredient;
            deleteIngredientFromAll(\%allergenMap, $ingredient);
            delete $allergenMap{$allergen};
        }
    }
}

my @allergens = sort keys %ingredientAllergen;
my @result;
foreach my $allergen (@allergens) {
    push @result, $ingredientAllergen{$allergen};
}

print join(",", @result) . "\n";

sub deleteIngredientFromAll {
    my ($allergenMap, $ingredient) = @_;
    foreach my $ingredients (values %$allergenMap) {
        delete $ingredients->{$ingredient};
    }
}
