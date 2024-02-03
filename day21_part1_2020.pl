
use strict;
use warnings;

my %allergenMap;
my %ingredientCount;
my %safeIngredients;

open(my $fh, '<', 'input.txt') or die $!;

while (my $line = <$fh>) {
    chomp $line;
    my @parts = split / \(contains /, $line;
    my @ingredients = split ' ', $parts[0];
    my @allergens = ();
    if (scalar @parts > 1) {
        @allergens = split ', ', substr($parts[1], 0, -1);
    }

    foreach my $ingredient (@ingredients) {
        $ingredientCount{$ingredient}++;
        $safeIngredients{$ingredient} = 1;
    }

    foreach my $allergen (@allergens) {
        if (!exists $allergenMap{$allergen}) {
            $allergenMap{$allergen} = {};
            foreach my $ingredient (@ingredients) {
                $allergenMap{$allergen}->{$ingredient} = 1;
            }
        } else {
            foreach my $ingredient (keys %{$allergenMap{$allergen}}) {
                if (!contains(\@ingredients, $ingredient)) {
                    delete $allergenMap{$allergen}->{$ingredient};
                }
            }
        }
    }
}

foreach my $ingredients (values %allergenMap) {
    foreach my $ingredient (keys %$ingredients) {
        delete $safeIngredients{$ingredient};
    }
}

my $count = 0;
foreach my $ingredient (keys %safeIngredients) {
    $count += $ingredientCount{$ingredient};
}

print "$count\n";

sub contains {
    my ($slice, $str) = @_;
    foreach my $v (@$slice) {
        if ($v eq $str) {
            return 1;
        }
    }
    return 0;
}
