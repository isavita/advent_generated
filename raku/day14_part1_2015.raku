#!/usr/bin/env raku

sub MAIN() {
    my $seconds = 2503;
    my @distances = "input.txt".IO.lines.map( {
        m/^ \w+ ' can fly ' (\d+) ' km/s for ' (\d+) ' seconds, but then must rest for ' (\d+) ' seconds.' $/;
        my ($speed, $fly, $rest) = $0.Int, $1.Int, $2.Int;
        my $cycle = $fly + $rest;
        my $full = $seconds div $cycle;
        my $remaining = $seconds % $cycle;
        $full * $speed * $fly + ($remaining min $fly) * $speed;
    } );
    say @distances.max;
}