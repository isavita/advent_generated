
my %name2id;
my @rules;
my $next-id = 0;

sub id-for(Str $name) {
    %name2id{$name} //= do {
        @rules.push: { name => $name, contains => [] };
        $next-id++;
        $next-id - 1;
    }
}

sub MAIN {
    for 'input.txt'.IO.lines {
        my $line = $_;
        my ($outer, $rest) = $line.split(' bags contain ');
        next unless $rest;

        my $outer-id = id-for($outer);
        next if $rest eq 'no other bags.';

        for $rest.split(',') {
            my @tok = .words;
            next unless @tok >= 3;
            my $inner-name = "@tok[1..2]";
            @rules[$outer-id]<contains>.push: id-for($inner-name);
        }
    }

    my $shiny = %name2id{'shiny gold'};
    unless defined $shiny {
        say 0;
        exit;
    }

    my %memo;
    sub can-contains-gold($id) {
        return True  if $id eq $shiny;
        return %memo{$id} if %memo{$id}:exists;
        for @rules[$id]<contains>.flat -> $c {
            return %memo{$id} = True if can-contains-gold($c);
        }
        False;
    }

    say (0..^$next-id).grep({ $_ ne $shiny && can-contains-gold($_) }).elems;
}
