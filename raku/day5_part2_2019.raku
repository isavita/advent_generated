
sub MAIN {
    my @instructions = 'input.txt'.IO.slurp.split(',').map(*.Int).Array;
    my $input-val = 5;
    my $index = 0;

    while $index < @instructions.elems {
        my $instruction = @instructions[$index];
        my $opcode = $instruction % 100;
        my $mode1 = $instruction div 100 % 10;
        my $mode2 = $instruction div 1000 % 10;

        if $opcode == 99 {
            last;
        }

        my $get-val = -> $param-index, $mode {
            my $ptr = @instructions[$index + $param-index];
            $mode == 0 ?? @instructions[$ptr] !! $ptr;
        };

        given $opcode {
            when 1 {
                my $val1 = $get-val.(1, $mode1);
                my $val2 = $get-val.(2, $mode2);
                my $dest = @instructions[$index + 3];
                @instructions[$dest] = $val1 + $val2;
                $index += 4;
            }
            when 2 {
                my $val1 = $get-val.(1, $mode1);
                my $val2 = $get-val.(2, $mode2);
                my $dest = @instructions[$index + 3];
                @instructions[$dest] = $val1 * $val2;
                $index += 4;
            }
            when 3 {
                my $dest = @instructions[$index + 1];
                @instructions[$dest] = $input-val;
                $index += 2;
            }
            when 4 {
                my $output-val = $get-val.(1, $mode1);
                say $output-val;
                $index += 2;
            }
            when 5 {
                my $val1 = $get-val.(1, $mode1);
                my $val2 = $get-val.(2, $mode2);
                if $val1 != 0 {
                    $index = $val2;
                } else {
                    $index += 3;
                }
            }
            when 6 {
                my $val1 = $get-val.(1, $mode1);
                my $val2 = $get-val.(2, $mode2);
                if $val1 == 0 {
                    $index = $val2;
                } else {
                    $index += 3;
                }
            }
            when 7 {
                my $val1 = $get-val.(1, $mode1);
                my $val2 = $get-val.(2, $mode2);
                my $dest = @instructions[$index + 3];
                @instructions[$dest] = $val1 < $val2 ?? 1 !! 0;
                $index += 4;
            }
            when 8 {
                my $val1 = $get-val.(1, $mode1);
                my $val2 = $get-val.(2, $mode2);
                my $dest = @instructions[$index + 3];
                @instructions[$dest] = $val1 == $val2 ?? 1 !! 0;
                $index += 4;
            }
        }
    }
}
