
sub MAIN {
    my %registers = <a b c d> Z=> 0;
    my @instructions = 'input.txt'.IO.slurp.lines;

    my $i = 0;
    while $i < @instructions.elems {
        my @parts = @instructions[$i].split(' ');
        given @parts[0] {
            when 'cpy' {
                my $val = @parts[1].Int // %registers{@parts[1]};
                %registers{@parts[2]} = $val;
            }
            when 'inc' {
                %registers{@parts[1]}++;
            }
            when 'dec' {
                %registers{@parts[1]}--;
            }
            when 'jnz' {
                my $val = @parts[1].Int // %registers{@parts[1]};
                if $val != 0 {
                    $i += @parts[2].Int;
                    next;
                }
            }
        }
        $i++;
    }

    %registers<a>.say;
}
