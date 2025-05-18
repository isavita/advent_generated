
use v6;

sub MAIN {
    my $steps = 'input.txt'.IO.slurp.Int;
    my @buffer = (0);
    my $current_pos = 0;
    for 1 .. 2017 -> $i {
        $current_pos = ($current_pos + $steps) % @buffer.elems + 1;
        @buffer.splice($current_pos, 0, $i);
    }
    say @buffer[($current_pos + 1) % @buffer.elems];
    $current_pos = 0;
    my $value_after_zero;
    for 1 .. 50000000 -> $i {
        $current_pos = ($current_pos + $steps) % $i + 1;
        if $current_pos == 1 {
            $value_after_zero = $i;
        }
    }
    say $value_after_zero;
}
