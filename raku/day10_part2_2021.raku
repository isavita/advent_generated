
sub syntax-error-score($c) {
    +%( ')' => 3, ']' => 57, '}' => 1197, '>' => 25137 ){ $c }
}

sub autocomplete-score(Str $s) {
    my $score = 0;
    for $s.comb -> $c {
        $score = $score * 5 + %( ')' => 1, ']' => 2, '}' => 3, '>' => 4 ){ $c }
    }
    $score
}

sub MAIN {
    my $syntax-error-score-total = 0;
    my @autocomplete-scores;

    for 'input.txt'.IO.lines -> $line {
        my @stack;
        my $corrupted = False;

        for $line.comb -> $c {
            if    $c eq '(' { @stack.push(')') }
            elsif $c eq '[' { @stack.push(']') }
            elsif $c eq '{' { @stack.push('}') }
            elsif $c eq '<' { @stack.push('>') }
            elsif !@stack || @stack.pop ne $c {
                $syntax-error-score-total += syntax-error-score($c);
                $corrupted = True;
                last
            }
        }

        unless $corrupted {
            my $autocomplete = @stack.reverse.join;
            @autocomplete-scores.push: autocomplete-score($autocomplete);
        }
    }

    say "Syntax error score: $syntax-error-score-total";

    @autocomplete-scores .= sort;
    say "Middle autocomplete score: " ~ @autocomplete-scores[@autocomplete-scores div 2]
}
