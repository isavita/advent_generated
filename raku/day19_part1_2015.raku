
sub MAIN {
    my %rules;
    my $target;

    for 'input.txt'.IO.lines {
        if /(.+) ' => ' (.+)/ {
            %rules{$0}.push: $1;
        } elsif .chars {
            $target = $_;
        }
    }

    my %seen;
    for %rules.kv -> $k, @v {
        for $target.match(/$k/, :overlap) -> $m {
            for @v -> $r {
                my $new = $target.substr(0, $m.from) ~ $r ~ $target.substr($m.to);
                %seen{$new} = True;
            }
        }
    }

    say %seen.elems;
}
