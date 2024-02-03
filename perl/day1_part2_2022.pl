
use strict;
use warnings;
use List::Util qw(sum);

open(my $fh, '<', 'input.txt') or die $!;

my @caloriesList;
my $currentCalories = 0;

while (my $line = <$fh>) {
    chomp $line;

    if ($line eq "") {
        push @caloriesList, $currentCalories;
        $currentCalories = 0;
        next;
    }

    my $calories = int($line);
    $currentCalories += $calories;
}

push @caloriesList, $currentCalories;
@caloriesList = sort { $b <=> $a } @caloriesList;

my $topThreeSum = 0;
for my $i (0..2) {
    $topThreeSum += $caloriesList[$i] if $i < scalar @caloriesList;
}

print $topThreeSum . "\n";
