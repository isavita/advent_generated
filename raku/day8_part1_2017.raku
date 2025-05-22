sub MAIN {
    my $fh = 'input.txt'.IO.open or die;

    my %registers;

    for $fh.lines -> $line {
        my ($reg, $op, $amount, $, $cond-reg, $cond-op, $cond-val) = $line.words;

        $amount = $amount.Int;
        $cond-val = $cond-val.Int;

        my $cond-met = False;
        given $cond-op {
            when '>'  { $cond-met = %registers{$cond-reg} >  $cond-val }
            when '>=' { $cond-met = %registers{$cond-reg} >= $cond-val }
            when '<'  { $cond-met = %registers{$cond-reg} <  $cond-val }
            when '<=' { $cond-met = %registers{$cond-reg} <= $cond-val }
            when '==' { $cond-met = %registers{$cond-reg} == $cond-val }
            when '!=' { $cond-met = %registers{$cond-reg} != $cond-val }
        }

        if $cond-met {
            given $op {
                when 'inc' { %registers{$reg} += $amount }
                when 'dec' { %registers{$reg} -= $amount }
            }
        }
    }

    my $maxValue = 0;
    for %registers.values -> $value {
        if $value > $maxValue {
            $maxValue = $value;
        }
    }

    $maxValue.say;
}