
my %reactions;
my %ingredients;
my %surplus;

sub parse-chemical(Str $s) {
    my ($amount, $name) = $s.split(' ');
    %( name => $name, amount => +$amount )
}

sub calculate-ore(Str $chem, Int $amount is copy) is rw {
    return $amount if $chem eq 'ORE';

    if (%surplus{$chem} // 0) >= $amount {
        %surplus{$chem} -= $amount;
        return 0;
    }

    $amount -= %surplus{$chem} // 0;
    %surplus{$chem} = 0;

    my $reaction = %reactions{$chem};
    my $times = ($amount / $reaction<amount>).ceiling;
    my $ore = 0;

    for %ingredients{$chem}.list -> $ing {
        $ore += calculate-ore($ing<name>, $ing<amount> * $times);
    }

    %surplus{$chem} += $times * $reaction<amount> - $amount;
    $ore
}

sub max-fuel(Int $ore-available) {
    my Int $low = 0;
    my Int $high = $ore-available;
    while $low < $high {
        my Int $mid = (($low + $high + 1) / 2).ceiling;
        %surplus = ();
        if calculate-ore('FUEL', $mid) > $ore-available {
            $high = $mid - 1;
        } else {
            $low = $mid;
        }
    }
    $low
}

for 'input.txt'.IO.lines {
    my ($ins, $out) = .split(' => ');
    my %out = parse-chemical($out);
    %reactions{%out<name>} = %out;
    %ingredients{%out<name>} = $ins.split(', ').map({ parse-chemical($_) }).list;
}

say max-fuel(1_000_000_000_000);
