sub MAIN {
    my $sum = 0;
    for "input.txt".IO.lines -> $line {
        next unless $line ~~ /^(.+)\-(\d+)\[(.+)\]$/;
        my ($name, $sector, $chk) = ~$0, +$1, ~$2;
        my $computed = $name.comb(/<[a..z]>/).Bag.sort({-.value,.key}).head(5).map(*.key).join('');
        $sum += $sector if $computed eq $chk;
    }
    say $sum;
}