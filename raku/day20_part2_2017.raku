
sub MAIN {
    my @particles = 'input.txt'.IO.lines.map: -> $line {
        my @nums = $line.comb(/ '-'? \d+ /).map: *.Int;
        { p => @nums[^3], v => @nums[3..5], a => @nums[6..8] }
    }

    for ^1000 {
        my %pos;
        %pos{"{@particles[$_]<p>}"}++ for ^@particles;

        @particles .= grep: -> $p {
            %pos{"{$p<p>}"} == 1
        }

        for @particles -> $p {
            $p<v>[$_] += $p<a>[$_] for ^3;
            $p<p>[$_] += $p<v>[$_] for ^3;
        }
    }

    say @particles
}
