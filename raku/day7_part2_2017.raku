
my %prog;

for 'input.txt'.IO.lines {
    my ($name, $w, *@kids) = m:g/<[a..z]>+|\d+/».Str;
    %prog{$name} = { weight => +$w, holds => @kids };
}

sub dfs($n) {
    my $p := %prog{$n};
    my $sum = $p<weight>;
    my %w;

    for $p<holds>.flat -> $c {
        my ($tot, $ok) = dfs($c);
        return (0, False) unless $ok;
        $sum += $tot;
        %w{$tot}++;
    }

    for %w.kv -> $v1, $c1 {
        for %w.kv -> $v2, $c2 {
            next if $v1 eq $v2;
            if $c1 < $c2 {
                my $bad = $p<holds>.first: { dfs($_)[0] == $v1 };
                say %prog{$bad}<weight> + ($v2 - $v1);
                return (0, False);
            }
        }
    }
    ($sum, True)
}

dfs('dtacyn');
