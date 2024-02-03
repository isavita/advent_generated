
use strict;
use warnings;

my @ingredients = readIngredients("input.txt");
my $maxScore = findMaxScore(\@ingredients, 100);
print "$maxScore\n";

sub readIngredients {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Error reading input: $!";
    
    my @ingredients;
    while (my $line = <$fh>) {
        my @parts = split(' ', $line);
        next if scalar @parts < 11;

        my $capacity = substr($parts[2], 0, -1);
        my $durability = substr($parts[4], 0, -1);
        my $flavor = substr($parts[6], 0, -1);
        my $texture = substr($parts[8], 0, -1);

        push @ingredients, {
            name => $parts[0],
            capacity => $capacity,
            durability => $durability,
            flavor => $flavor,
            texture => $texture
        };
    }

    close $fh;
    return @ingredients;
}

sub findMaxScore {
    my ($ingredients, $totalTeaspoons) = @_;
    return calculateMaxScore($ingredients, 0, $totalTeaspoons, []);
}

sub calculateMaxScore {
    my ($ingredients, $index, $remaining, $teaspoons) = @_;
    if ($index == scalar @$ingredients - 1) {
        push @$teaspoons, $remaining;
        return score($ingredients, $teaspoons);
    }

    my $maxScore = 0;
    for my $i (0..$remaining) {
        my $score = calculateMaxScore($ingredients, $index+1, $remaining-$i, [@$teaspoons, $i]);
        $maxScore = $score if $score > $maxScore;
    }
    return $maxScore;
}

sub score {
    my ($ingredients, $teaspoons) = @_;
    my ($capacity, $durability, $flavor, $texture) = (0, 0, 0, 0);
    for my $i (0..$#{$ingredients}) {
        $capacity += $ingredients->[$i]{capacity} * $teaspoons->[$i];
        $durability += $ingredients->[$i]{durability} * $teaspoons->[$i];
        $flavor += $ingredients->[$i]{flavor} * $teaspoons->[$i];
        $texture += $ingredients->[$i]{texture} * $teaspoons->[$i];
    }

    $capacity = 0 if $capacity < 0;
    $durability = 0 if $durability < 0;
    $flavor = 0 if $flavor < 0;
    $texture = 0 if $texture < 0;

    return $capacity * $durability * $flavor * $texture;
}
