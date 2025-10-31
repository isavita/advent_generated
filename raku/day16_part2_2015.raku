sub MAIN() {
    my %mfcsam =
        children => 3,
        cats => 7,
        samoyeds => 2,
        pomeranians => 3,
        akitas => 0,
        vizslas => 0,
        goldfish => 5,
        trees => 3,
        cars => 2,
        perfumes => 1;

    for "input.txt".IO.lines -> $line {
        next unless $line ~~ /^^ 'Sue ' (\d+) ':' \s* (.*) $$/;
        my $sue = +$0;
        my $valid = True;
        for $1.split(', ') -> $p {
            next unless $p ~~ /(\w+) ':' \s* (\d+)/;
            my $name = ~$0;
            my $value = +$1;
            my $target = %mfcsam{$name};
            if $name eq any <cats trees> {
                $valid = False unless $target < $value;
            } elsif $name eq any <pomeranians goldfish> {
                $valid = False unless $target > $value;
            } else {
                $valid = False unless $target == $value;
            }
            last unless $valid;
        }
        if $valid {
            say $sue;
            last;
        }
    }
}