
sub calculate-rates(@numbers) {
    my $bit-length = @numbers[0].chars;
    my $gamma-str = '';
    my $epsilon-str = '';

    for ^$bit-length -> $i {
        my $count0 = 0;
        my $count1 = 0;

        for @numbers -> $num {
            if $num.substr($i, 1) eq '0' {
                $count0++;
            } else {
                $count1++;
            }
        }

        $gamma-str ~= ($count1 > $count0 ?? '1' !! '0');
        $epsilon-str ~= ($count1 < $count0 ?? '1' !! '0');
    }

    my $gamma-rate = :2($gamma-str);
    my $epsilon-rate = :2($epsilon-str);

    return $gamma-rate, $epsilon-rate;
}

sub MAIN() {
    my @data = 'input.txt'.IO.lines;
    my ($gamma-rate, $epsilon-rate) = calculate-rates(@data);
    say $gamma-rate * $epsilon-rate;
}

