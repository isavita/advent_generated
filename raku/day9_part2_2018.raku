
use v6;

sub main {
    my $f = open 'input.txt', :r;
    my @words = $f.get.words;
    $f.close;

    my $players = @words[0].Int;
    my $last-marble-base = @words[6].Int;
    my $last-marble = $last-marble-base * 100;

    my @scores = (0) xx $players;

    my %circle;
    my $current-marble = 0;
    %circle{$current-marble} = { 'next' => $current-marble, 'prev' => $current-marble };

    for 1..$last-marble -> $marble {
        my $player = ($marble - 1) % $players;

        if $marble % 23 == 0 {
            @scores[$player] += $marble;

            my $to-remove = $current-marble;
            for 1..7 {
                $to-remove = %circle{$to-remove}<prev>;
            }

            @scores[$player] += $to-remove;

            my $prev-of-removed = %circle{$to-remove}<prev>;
            my $next-of-removed = %circle{$to-remove}<next>;
            %circle{$prev-of-removed}<next> = $next-of-removed;
            %circle{$next-of-removed}<prev> = $prev-of-removed;

            $current-marble = $next-of-removed;
        } else {
            my $pos-before-insert = %circle{$current-marble}<next>;
            my $pos-after-insert = %circle{$pos-before-insert}<next>;

            %circle{$pos-before-insert}<next> = $marble;
            %circle{$pos-after-insert}<prev> = $marble;
            %circle{$marble} = { 'next' => $pos-after-insert, 'prev' => $pos-before-insert };

            $current-marble = $marble;
        }
    }

    say @scores.max;
}

main;
