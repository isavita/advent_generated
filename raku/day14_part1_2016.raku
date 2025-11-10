#!/usr/bin/env raku
use v6;
use Digest::MD5;

my $salt = "input.txt".IO.slurp.trim;
my %cache;

sub get-hash(Int $i --> Str) {
    return %cache{$i} if %cache{$i}:exists;
    %cache{$i} = md5($salt ~ $i).list.fmt("%02x", "");
}

my $found = 0;
my $answer;

for 0..* -> $idx {
    my $h = get-hash($idx);
    my $triplet;
    for 0..29 -> $p {
        my $c = substr($h, $p, 1);
        if substr($h, $p+1, 1) eq $c && substr($h, $p+2, 1) eq $c {
            $triplet = $c;
            last;
        }
    }
    if $triplet {
        my $pattern = $triplet x 5;
        for 1..1000 {
            if get-hash($idx + $_).contains($pattern) {
                $found++;
                last;
            }
        }
    }
    if $found == 64 {
        $answer = $idx;
        last;
    }
}
say $answer;