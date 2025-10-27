
my %bag;
my $shiny-gold;

for 'input.txt'.IO.lines {
    /(.+) ' bags contain ' (.+) / or next;
    my $outer = ~$0;
    my @inners;
    for $1.split(',') {
        /(\d+) \s (.+) \s bag/ and push @inners, ($1 => +$0);
    }
    %bag{$outer} = @inners;
    $shiny-gold //= %bag.keys.first(* eq 'shiny gold');
}

sub can-gold($color) {
    state %cache;
    %cache{$color} //= do {
        $color eq 'shiny gold' or
        %bag{$color}.first: { can-gold(.key) }
    }
}

sub bags-inside($color) {
    state %cache;
    %cache{$color} //= do {
        sum %bag{$color}.map: { .value * (1 + bags-inside(.key)) }
    }
}

say +%bag.keys.grep: { $_ ne $shiny-gold and can-gold($_) };
say bags-inside($shiny-gold);
