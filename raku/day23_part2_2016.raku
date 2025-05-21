
sub read-input() {
    'input.txt'.IO.lines.map(*.trim).Array
}

sub is-register($x) {
    $x eq 'a' || $x eq 'b' || $x eq 'c' || $x eq 'd'
}

sub get-value($x, %registers) {
    if is-register($x) {
        %registers{$x}
    } else {
        $x.Int
    }
}

sub execute-program(@instructions is raw, %registers is raw) {
    my $i = 0;
    my $len = @instructions.elems;

    while $i < $len {
        if ($i + 5 < $len) {
            my @pattern = @instructions[$i .. $i+5];
            if (@pattern[0].starts-with('cpy') &&
                @pattern[1].starts-with('inc') &&
                @pattern[2].starts-with('dec') &&
                @pattern[3].starts-with('jnz') &&
                @pattern[4].starts-with('dec') &&
                @pattern[5].starts-with('jnz')) {

                my @p0 = @pattern[0].split(' ');
                my @p1 = @pattern[1].split(' ');
                my @p2 = @pattern[2].split(' ');
                my @p3 = @pattern[3].split(' ');
                my @p4 = @pattern[4].split(' ');
                my @p5 = @pattern[5].split(' ');

                my ($cpy_x, $cpy_y) = @p0[1,2];
                my $inc_a = @p1[1];
                my $dec_c = @p2[1];
                my ($jnz_c, $jnz_c-offset) = @p3[1,2];
                my $dec_d = @p4[1];
                my ($jnz_d, $jnz_d-offset) = @p5[1,2];

                if ($inc_a eq 'a' && $dec_c eq $cpy_y && $jnz_c eq $cpy_y && $jnz_c-offset.Int == -2 &&
                    $dec_d eq 'd' && $jnz_d eq 'd' && $jnz_d-offset.Int == -5) {

                    %registers<a> += get-value($cpy_x, %registers) * %registers<d>;
                    %registers{$cpy_y} = 0;
                    %registers<d> = 0;
                    $i += 6;
                    next;
                }
            }
        }

        my @parts = @instructions[$i].split(' ');
        my $cmd = @parts[0];

        if ($cmd eq 'tgl') {
            my $x = get-value(@parts[1], %registers);
            my $target-idx = $i + $x;

            if (0 <= $target-idx < $len) {
                my @target-parts = @instructions[$target-idx].split(' ');
                my $target-cmd = @target-parts[0];
                if (@target-parts.elems == 2) {
                    @target-parts[0] = $target-cmd eq 'inc' ?? 'dec' !! 'inc';
                } elsif (@target-parts.elems == 3) {
                    @target-parts[0] = $target-cmd eq 'jnz' ?? 'cpy' !! 'jnz';
                }
                @instructions[$target-idx] = @target-parts.join(' ');
            }
            $i++;
            next;
        }

        given $cmd {
            when 'cpy' {
                my ($x, $y) = @parts[1,2];
                if is-register($y) {
                    %registers{$y} = get-value($x, %registers);
                }
                $i++;
            }
            when 'inc' {
                my $x = @parts[1];
                if is-register($x) {
                    %registers{$x}++;
                }
                $i++;
            }
            when 'dec' {
                my $x = @parts[1];
                if is-register($x) {
                    %registers{$x}--;
                }
                $i++;
            }
            when 'jnz' {
                my ($x, $y) = @parts[1,2];
                if get-value($x, %registers) != 0 {
                    $i += get-value($y, %registers);
                } else {
                    $i++;
                }
            }
            default {
                $i++;
            }
        }
    }
}

sub MAIN() {
    my @instructions = read-input();
    my %registers = <a b c d> Z=> (12, 0, 0, 0);
    execute-program(@instructions, %registers);
    say %registers<a>;
}
