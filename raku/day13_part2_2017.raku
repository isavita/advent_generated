#!/usr/bin/env raku
use v6;

sub MAIN {
    my $fh = open "input.txt", :r or die "Cannot open input.txt: $!";
    my @layers = gather {
        for $fh.lines -> $line {
            if $line ~~ m/ ^ (\d+) \s* ':' \s* (\d+) $ / {
                take ( +$0, +$1 );
            }
        }
    };
    $fh.close;
    my $delay = 0;
    loop {
        my $caught = False;
        for @layers -> ($depth, $range) {
            my $cycle = ($range - 1) * 2;
            if ( ($depth + $delay) % $cycle == 0 ) {
                $caught = True;
                last;
            }
        }
        last unless $caught;
        $delay++;
    }
    say $delay;
}