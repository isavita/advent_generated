
sub main {
    my @capacities;
    my @durabilities;
    my @flavors;
    my @textures;

    for "input.txt".IO.lines -> $line {
        my @nums = $line ~~ m:g/ (\-?\d+) /;
        push @capacities, @nums[0];
        push @durabilities, @nums[1];
        push @flavors, @nums[2];
        push @textures, @nums[3];
    }

    my $max-score = 0;
    my $total-teaspoons = 100;

    for 0..$total-teaspoons -> $t0 {
        for 0..$total-teaspoons-$t0 -> $t1 {
            for 0..$total-teaspoons-$t0-$t1 -> $t2 {
                my $t3 = $total-teaspoons - $t0 - $t1 - $t2;

                my $capacity = @capacities[0] * $t0 + @capacities[1] * $t1 + @capacities[2] * $t2 + @capacities[3] * $t3;
                my $durability = @durabilities[0] * $t0 + @durabilities[1] * $t1 + @durabilities[2] * $t2 + @durabilities[3] * $t3;
                my $flavor = @flavors[0] * $t0 + @flavors[1] * $t1 + @flavors[2] * $t2 + @flavors[3] * $t3;
                my $texture = @textures[0] * $t0 + @textures[1] * $t1 + @textures[2] * $t2 + @textures[3] * $t3;

                $capacity = max(0, $capacity);
                $durability = max(0, $durability);
                $flavor = max(0, $flavor);
                $texture = max(0, $texture);

                $max-score = max($max-score, $capacity * $durability * $flavor * $texture);
            }
        }
    }

    say $max-score;
}

main;
