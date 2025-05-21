
sub MAIN {
    my %memory = 'input.txt'.IO.slurp.split(',').map(*.Int).kv;

    my $ip = 0;
    my $relative-base = 0;
    my $input-val = 2;
    my $output = 0;

    sub get-param-val($offset, $modes-val) {
        my $p = %memory{$ip + $offset} // 0;
        my $mode = ($modes-val div 10**($offset-1)) % 10;

        given $mode {
            when 0 { %memory{$p} // 0 }
            when 1 { $p }
            when 2 { %memory{$relative-base + $p} // 0 }
            default { die "Unknown parameter mode: $mode" }
        }
    }

    sub set-param-val($offset, $value, $modes-val) {
        my $p-addr = %memory{$ip + $offset} // 0;
        my $mode = ($modes-val div 10**($offset-1)) % 10;

        given $mode {
            when 0 { %memory{$p-addr} = $value }
            when 2 { %memory{$relative-base + $p-addr} = $value }
            default { die "Unknown parameter mode for write: $mode" }
        }
    }

    loop {
        my $instruction = %memory{$ip} // 0;
        my $opcode = $instruction % 100;
        my $modes-val = $instruction div 100;

        given $opcode {
            when 1 {
                my $val1 = get-param-val(1, $modes-val);
                my $val2 = get-param-val(2, $modes-val);
                set-param-val(3, $val1 + $val2, $modes-val);
                $ip += 4;
            }
            when 2 {
                my $val1 = get-param-val(1, $modes-val);
                my $val2 = get-param-val(2, $modes-val);
                set-param-val(3, $val1 * $val2, $modes-val);
                $ip += 4;
            }
            when 3 {
                set-param-val(1, $input-val, $modes-val);
                $ip += 2;
            }
            when 4 {
                $output = get-param-val(1, $modes-val);
                $ip += 2;
            }
            when 5 {
                my $val1 = get-param-val(1, $modes-val);
                my $val2 = get-param-val(2, $modes-val);
                $ip = $val1 != 0 ?? $val2 !! $ip + 3;
            }
            when 6 {
                my $val1 = get-param-val(1, $modes-val);
                my $val2 = get-param-val(2, $modes-val);
                $ip = $val1 == 0 ?? $val2 !! $ip + 3;
            }
            when 7 {
                my $val1 = get-param-val(1, $modes-val);
                my $val2 = get-param-val(2, $modes-val);
                set-param-val(3, $val1 < $val2 ?? 1 !! 0, $modes-val);
                $ip += 4;
            }
            when 8 {
                my $val1 = get-param-val(1, $modes-val);
                my $val2 = get-param-val(2, $modes-val);
                set-param-val(3, $val1 == $val2 ?? 1 !! 0, $modes-val);
                $ip += 4;
            }
            when 9 {
                $relative-base += get-param-val(1, $modes-val);
                $ip += 2;
            }
            when 99 {
                last;
            }
            default {
                die "Unknown opcode: $opcode at address $ip";
            }
        }
    }
    say $output;
}
