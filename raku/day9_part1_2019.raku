
sub MAIN {
    my @input = slurp('input.txt').trim.split(',')».Int;
    my %mem = @input.kv.map: { $^k => $^v };
    my int $ip           = 0;
    my int $base         = 0;
    my int $out          = 0;

    while 1 {
        my int $op  = %mem{$ip} % 100;
        my @m      = (%mem{$ip} div 100).Str.comb.reverse;

        sub mode(int $p) { @m[$p - 1] // 0 }

        sub get(int $offset) {
            my int $v = %mem{$ip + $offset} // 0;
            given mode($offset) {
                when 0 { %mem{$v} // 0 }
                when 1 { $v }
                when 2 { %mem{$base + $v} // 0 }
            }
        }

        sub set(int $offset, int $val) {
            my int $a = %mem{$ip + $offset} // 0;
            given mode($offset) {
                when 0 { %mem{$a} = $val }
                when 2 { %mem{$base + $a} = $val }
            }
        }

        given $op {
            when 1  { set(3, get(1) + get(2)); $ip += 4 }
            when 2  { set(3, get(1) * get(2)); $ip += 4 }
            when 3  { set(1, 1); $ip += 2 }
            when 4  { $out = get(1); $ip += 2 }
            when 5  { $ip = get(1) ?? get(2) !! $ip + 3 }
            when 6  { $ip = get(1) ?? $ip + 3 !! get(2) }
            when 7  { set(3, get(1) < get(2) ?? 1 !! 0); $ip += 4 }
            when 8  { set(3, get(1) == get(2) ?? 1 !! 0); $ip += 4 }
            when 9  { $base += get(1); $ip += 2 }
            when 99 { put $out; exit }
            default  { die "unknown opcode $op" }
        }
    }
}
