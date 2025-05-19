
sub react_polymer(Str $polymer) {
    my @stack;
    for $polymer.comb('') -> $unit {
        if @stack.elems > 0 && $unit.lc eq @stack.tail.lc && $unit ne @stack.tail {
            @stack.pop;
        } else {
            @stack.push($unit);
        }
    }
    @stack.elems
}

sub main {
    my $polymer = 'input.txt'.IO.slurp.chomp;
    react_polymer($polymer).say;
}

main;
