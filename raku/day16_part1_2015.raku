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
        perfumes => 1,
    ;

    for "input.txt".IO.lines -> $line {
        next unless $line ~~ /^ "Sue " (\d+) ": " (.+)$/;
        my $sue-num = +$0;
        my @pairs = $1.split(', ');
        my $match = True;
        for @pairs -> $pair {
            next unless $pair ~~ /^ (\w+) ": " (\d+)$/;
            my ($attr, $val) = (~$0, +$1);
            if %mfcsam{$attr} != $val {
                $match = False;
                last;
            }
        }
        if $match {
            say $sue-num;
            last;
        }
    }
}