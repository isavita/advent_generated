sub MAIN() {
    my @ingredients;
    for "input.txt".IO.lines -> $line {
        if $line ~~ /
            ^ (.+?) ':' \s*
            'capacity' \s* (\-?\d+) ',' \s*
            'durability' \s* (\-?\d+) ',' \s*
            'flavor' \s* (\-?\d+) ',' \s*
            'texture' \s* (\-?\d+) ',' \s*
            'calories' \s* (\-?\d+)
            $ / {
            my ($cap, $dur, $flav, $text, $cal) = $/[1..5]>>.Int;
            @ingredients.push([ $cap, $dur, $flav, $text, $cal ]);
        }
    }

    my $target-teaspoons = 100;
    my $target-calories = 500;
    my $max-score = 0;
    my @alloc = 0 xx @ingredients;

    sub compute-calories(@alloc) {
        my $cal = 0;
        for ^@ingredients -> $i {
            $cal += @ingredients[$i][4] * @alloc[$i];
        }
        $cal;
    }

    sub compute-score(@alloc) {
        my ($capacity, $durability, $flavor, $texture) = (0,0,0,0);
        for ^@ingredients -> $i {
            $capacity   += @ingredients[$i][0] * @alloc[$i];
            $durability += @ingredients[$i][1] * @alloc[$i];
            $flavor     += @ingredients[$i][2] * @alloc[$i];
            $texture    += @ingredients[$i][3] * @alloc[$i];
        }
        $capacity   max= 0;
        $durability max= 0;
        $flavor     max= 0;
        $texture    max= 0;
        $capacity * $durability * $flavor * $texture;
    }

    sub dfs($i, $remaining) {
        if $i == @ingredients - 1 {
            @alloc[$i] = $remaining;
            if compute-calories(@alloc) == $target-calories {
                $max-score max= compute-score(@alloc);
            }
            return;
        }
        for 0..$remaining -> $x {
            @alloc[$i] = $x;
            dfs($i+1, $remaining - $x);
        }
    }

    dfs(0, $target-teaspoons);
    say $max-score;
}