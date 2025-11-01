
my %pairings = ')' => '(', ']' => '[', '}' => '{', '>' => '<';
my %scores   = ')' => 3,   ']' => 57,  '}' => 1197, '>' => 25137;

sub check-line(Str $line) {
    my @stack;
    for $line.comb -> $c {
        given $c {
            when '(' | '[' | '{' | '<' { @stack.push: $c }
            when %pairings {
                return %scores{$c} unless @stack and @stack.tail eq %pairings{$c};
                @stack.pop;
            }
        }
    }
    0
}

sub MAIN {
    my $total = 0;
    for 'input.txt'.IO.lines {
        $total += check-line($_);
    }
    say $total;
}
