use strict;
use warnings;

my $input = do {
    local $/;
    open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
    <$fh>;
};
chomp $input;

my $result = solve($input);
print "$result\n";

sub solve {
    my ($input) = @_;
    my @positions = parse_input($input);
    my %memo;
    my ($w1, $w2) = play($positions[0], $positions[1], 0, 0, 3, 1, \%memo);
    return $w1 > $w2 ? $w1 : $w2;
}

sub play {
    my ($p1, $p2, $s1, $s2, $rolls_left, $is_p1_turn, $memo) = @_;
    my $key = "$p1,$p2,$s1,$s2,$rolls_left,$is_p1_turn";
    return @{$memo->{$key}} if exists $memo->{$key};

    my $player_index = $is_p1_turn ? 0 : 1;
    my @scores = ($s1, $s2);

    if ($rolls_left == 0) {
        $scores[$player_index] += $player_index == 0 ? $p1 : $p2;
        return (1, 0) if $scores[$player_index] >= 21 && $player_index == 0;
        return (0, 1) if $scores[$player_index] >= 21 && $player_index == 1;
        $is_p1_turn = !$is_p1_turn;
        $rolls_left = 3;
        $player_index = 1 - $player_index;
    }

    my ($wins1, $wins2) = (0, 0);
    for my $roll (1..3) {
        my $new_pos = $player_index == 0 ? $p1 + $roll : $p2 + $roll;
        $new_pos -= 10 if $new_pos > 10;
        my ($r1, $r2) = play($player_index == 0 ? $new_pos : $p1, $player_index == 1 ? $new_pos : $p2,
                             $scores[0], $scores[1], $rolls_left - 1, $is_p1_turn, $memo);
        $wins1 += $r1;
        $wins2 += $r2;
    }

    $memo->{$key} = [$wins1, $wins2];
    return ($wins1, $wins2);
}

sub parse_input {
    my ($input) = @_;
    my @positions;
    while ($input =~ /Player \d+ starting position: (\d+)/g) {
        push @positions, $1;
    }
    return @positions;
}