
sub josephus(Int $n) {
    my $base3 = $n.base(3);
    my $first-digit = $base3.substr(0, 1);
    my $rest-digits = $base3.substr(1);

    if $first-digit eq '1' {
        return $rest-digits.parse-base(3);
    } else {
        return ('1' ~ $rest-digits).parse-base(3) + $rest-digits.parse-base(3);
    }
}

sub MAIN {
    my $num-elves = slurp('input.txt').Int;
    say josephus($num-elves);
}
