
sub dragon_curve(Str:D $a) {
    my $b = $a.flip.trans('0' => '1', '1' => '0');
    $a ~ '0' ~ $b
}

sub checksum(Str:D $data) {
    my @result_chars;
    for $data.comb(2) -> $pair {
        my ($c1, $c2) = $pair.comb;
        @result_chars.push: ($c1 eq $c2 ?? '1' !! '0');
    }
    my $result = @result_chars.join;
    return checksum($result) if $result.chars % 2 == 0;
    $result
}

sub MAIN() {
    my Str $initial_state = 'input.txt'.IO.slurp.trim;
    my Int constant TARGET_LENGTH = 272;

    my Str $data = $initial_state;
    while $data.chars < TARGET_LENGTH {
        $data = dragon_curve($data);
    }

    $data = $data.substr(0, TARGET_LENGTH);
    say checksum($data);
}
