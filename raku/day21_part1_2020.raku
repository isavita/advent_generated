
sub MAIN {
    my %count;        # ingredient → total occurrences
    my %candidate;    # allergen   → Set of possible ingredients

    for 'input.txt'.IO.lines -> $line {
        next unless $line ~~ /^(.+?) \s+ '(' \s* 'contains' \s+ (.+?) ')' $/;
        my @ingredients = $0.words;
        my @allergens   = $1.split(',').map: *.trim;

        for @ingredients { %count{$_}++ }
        my $ing-set = Set(@ingredients);
        for @allergens -> $a {
            %candidate{$a}:exists
                ?? (%candidate{$a} = %candidate{$a} ∩ $ing-set)
                !! (%candidate{$a} = $ing-set);
        }
    }

    my $allergenic = Set.new: %candidate.values.map(*.keys).flat;

    my $safe = 0;
    for %count.kv -> $ing, $cnt { $safe += $cnt if $ing ∉ $allergenic }
    say $safe;
}
