
sub evaluate(Str $expr --> Int) {
    my @tokens = $expr.comb: /\d+ | <[+*()]>/;
    my @out;
    my @ops;

    sub prec($op) { $op eq '+'|'*' ?? 1 !! 0 }

    sub apply() {
        my $b = @out.pop;
        my $a = @out.pop;
        @out.push: @ops.pop eq '+' ?? $a + $b !! $a * $b;
    }

    for @tokens -> $t {
        if $t ~~ /^ \d+ $/ { @out.push: $t }
        elsif $t eq '('   { @ops.push: $t }
        elsif $t eq ')'   { apply while @ops and @ops[*-1] ne '('; @ops.pop }
        else {
            apply while @ops and prec(@ops[*-1]) >= prec($t);
            @ops.push: $t;
        }
    }
    apply while @ops;
    @out[0];
}

sub MAIN() {
    my $total = 0;
    for 'input.txt'.IO.lines -> $line { $total += evaluate($line) }
    say $total;
}
