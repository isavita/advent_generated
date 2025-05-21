
use v6;

sub swap_position(Str $password, Int $x, Int $y) {
    my @p = $password.comb;
    (@p[$x], @p[$y]) = (@p[$y], @p[$x]);
    @p.join
}

sub swap_letter(Str $password, Str $x, Str $y) {
    $password.comb.map({
        when $_ eq $x { $y }
        when $_ eq $y { $x }
        default { $_ }
    }).join
}

sub rotate_left(Str $password, Int $steps) {
    my @p = $password.comb;
    @p.rotate($steps % @p.elems).join
}

sub rotate_right(Str $password, Int $steps) {
    my @p = $password.comb;
    @p.rotate(-($steps % @p.elems)).join
}

sub rotate_based_on_position(Str $password, Str $x) {
    my $index = $password.index($x);
    my $steps = 1 + $index + ($index >= 4 ?? 1 !! 0);
    rotate_right($password, $steps)
}

sub reverse_positions(Str $password, Int $x, Int $y) {
    ($x, $y) = ($y, $x) if $x > $y;

    my @p = $password.comb;
    @p[$x..$y] = @p[$x..$y].reverse;
    @p.join
}

sub move_position(Str $password, Int $x, Int $y) {
    my @p = $password.comb;
    my $char = @p.splice($x, 1)[0];
    @p.splice($y, 0, $char);
    @p.join
}

sub apply_operation(Str $op, Str $password) {
    my @fields = $op.split(' ');
    given @fields[0] {
        when "swap" {
            given @fields[1] {
                when "position" {
                    swap_position($password, @fields[2].Int, @fields[5].Int)
                }
                when "letter" {
                    swap_letter($password, @fields[2], @fields[5])
                }
            }
        }
        when "rotate" {
            given @fields[1] {
                when "left" {
                    rotate_left($password, @fields[2].Int)
                }
                when "right" {
                    rotate_right($password, @fields[2].Int)
                }
                when "based" {
                    rotate_based_on_position($password, @fields[6])
                }
            }
        }
        when "reverse" {
            reverse_positions($password, @fields[2].Int, @fields[4].Int)
        }
        when "move" {
            move_position($password, @fields[2].Int, @fields[5].Int)
        }
    }
}

sub MAIN() {
    my @operations = 'input.txt'.IO.slurp.lines.grep(*.chars);
    my $password = "abcdefgh";

    for @operations -> $op {
        $password = apply_operation($op, $password);
    }

    say $password;
}
