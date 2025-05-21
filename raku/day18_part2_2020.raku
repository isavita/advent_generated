
sub evaluate_simple(Str $expression) {
    my @parts = $expression.comb(/\d+|\+|\*/);
    my $total = @parts.shift.Int;
    for @parts.rotor(2) -> ($op, $num) {
        given $op {
            when '+' { $total += $num.Int }
            when '*' { $total *= $num.Int }
        }
    }
    $total
}

sub evaluate_advanced(Str $expression) {
    my @parts = $expression.comb(/\d+|\+|\*/);

    while '+' (elem) @parts {
        my $i = @parts.first('+', :k);
        my $result = @parts[$i - 1].Int + @parts[$i + 1].Int;
        @parts.splice($i - 1, 3, $result.Str);
    }

    my $total = @parts.shift.Int;
    for @parts.rotor(2) -> ($op, $num) {
        $total *= $num.Int;
    }
    $total
}

sub evaluate_expression(Str $expression, &evaluate_fn) {
    my $exp = $expression;
    while $exp ~~ m/\(/ {
        my $start = $exp.rindex('(');
        my $end = $exp.substr($start).index(')') + $start;
        my $inner = $exp.substr($start + 1, $end - ($start + 1));
        my $result = evaluate_fn($inner);
        $exp = $exp.substr(0, $start) ~ $result ~ $exp.substr($end + 1);
    }
    evaluate_fn($exp)
}

sub MAIN() {
    my @expressions = 'input.txt'.IO.slurp.lines.map(*.trim);

    my $result_part1 = [+] @expressions.map: { evaluate_expression($_, &evaluate_simple) };
    my $result_part2 = [+] @expressions.map: { evaluate_expression($_, &evaluate_advanced) };

    say $result_part1;
    say $result_part2;
}
