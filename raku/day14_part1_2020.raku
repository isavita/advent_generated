
sub MAIN() {
    my $mask = '';
    my %mem;

    for 'input.txt'.IO.lines {
        if /mask \s* '=' \s* (.*)/ {
            $mask = ~$0;
        }
        elsif /mem '[' (\d+) ']' \s* '=' \s* (\d+)/ {
            my $val = +$1;
            for $mask.comb.kv -> $i, $c {
                given $c {
                    when '1' { $val +|= 1 +< (35 - $i) }
                    when '0' { $val +&= +^ (1 +< (35 - $i)) }
                }
            }
            %mem{+$0} = $val;
        }
    }

    say [+] %mem.values;
}
