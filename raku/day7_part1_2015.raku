
sub MAIN {
    my %wires;
    my @instr;

    for 'input.txt'.IO.lines {
        my ($expr, $wire) = split(' -> ', $_);
        @instr.push: ($expr, $wire);
    }

    sub signal(Str $w) {
        return +$w if $w ~~ /^\d+$/;
        return %wires{$w} if %wires{$w}:exists;

        my $v;
        for @instr -> ($expr, $wire) {
            next unless $wire eq $w;

            given $expr {
                when /^\s*(\d+)\s+AND\s+(\S+)\s*$/ {
                    $v = (+$0 +& signal(~$1)) +& 0xFFFF;
                }
                when /^\s*(\S+)\s+AND\s+(\S+)\s*$/ {
                    $v = (signal(~$0) +& signal(~$1)) +& 0xFFFF;
                }
                when /^\s*(\S+)\s+OR\s+(\S+)\s*$/ {
                    $v = (signal(~$0) +| signal(~$1)) +& 0xFFFF;
                }
                when /^\s*(\S+)\s+LSHIFT\s+(\d+)\s*$/ {
                    $v = (signal(~$0) +< +$1) +& 0xFFFF;
                }
                when /^\s*(\S+)\s+RSHIFT\s+(\d+)\s*$/ {
                    $v = (signal(~$0) +> +$1) +& 0xFFFF;
                }
                when /^\s*NOT\s+(\S+)\s*$/ {
                    $v = (+^signal(~$0)) +& 0xFFFF;
                }
                default {
                    $v = signal($expr);
                }
            }
            %wires{$wire} = $v;
            return $v;
        }
        die "unknown wire $w";
    }

    say signal('a');
}
