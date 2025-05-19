
use v6;

sub parse-input($input) {
    my ($p1-pos, $p2-pos);
    for $input.lines -> $line {
        if $line ~~ / 'Player 1 starting position: ' (\d+) / {
            $p1-pos = +$0;
        } elsif $line ~~ / 'Player 2 starting position: ' (\d+) / {
            $p2-pos = +$0;
        }
    }
    return ($p1-pos, $p2-pos).List;
}

my %roll-frequencies = (
    3 => 1,
    4 => 3,
    5 => 6,
    6 => 7,
    7 => 6,
    8 => 3,
    9 => 1,
);

my %memo;

multi sub play($p1, $p2, $s1, $s2, $is-player1-turn --> List) {
    my $key = "$p1,$p2,$s1,$s2,$is-player1-turn";
    return %memo{$key} if %memo{$key}:exists;

    if $s1 >= 21 { return (1, 0).List }
    if $s2 >= 21 { return (0, 1).List }

    my ($total-wins1, $total-wins2) = (0, 0);
    my $idx = $is-player1-turn ?? 0 !! 1;

    for %roll-frequencies.kv -> $roll-sum, $frequency {
        my $current-pos = $idx == 0 ?? $p1 !! $p2;
        my $new-pos = ($current-pos + $roll-sum - 1) % 10 + 1;

        my $current-score = $idx == 0 ?? $s1 !! $s2;
        my $new-score = $current-score + $new-pos;

        my ($next-p1, $next-p2) = $idx == 0 ?? ($new-pos, $p2) !! ($p1, $new-pos);
        my ($next-s1, $next-s2) = $idx == 0 ?? ($new-score, $s2) !! ($s1, $new-score);

        if $new-score >= 21 {
            if $idx == 0 {
                $total-wins1 += $frequency;
            } else {
                $total-wins2 += $frequency;
            }
        } else {
            my ($w1, $w2) = play($next-p1, $next-p2, $next-s1, $next-s2, !$is-player1-turn).List;
            $total-wins1 += $w1 * $frequency;
            $total-wins2 += $w2 * $frequency;
        }
    }

    return %memo{$key} = ($total-wins1, $total-wins2).List;
}

sub MAIN() {
    my $input = slurp 'input.txt';
    my ($p1-start, $p2-start) = parse-input($input);

    my ($wins1, $wins2) = play($p1-start, $p2-start, 0, 0, True).List;

    say max($wins1, $wins2);
}
