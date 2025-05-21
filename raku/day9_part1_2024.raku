
sub MAIN() {
    my $line = 'input.txt'.IO.slurp.trim;

    my @disk;
    my $file-id = 0;
    my $is-file = True;
    for $line.comb -> $char {
        my $length = $char.Int;
        if $is-file {
            @disk.append: ($file-id.Str) xx $length;
            $file-id++;
        } else {
            @disk.append: ('.') xx $length;
        }
        $is-file = !$is-file;
    }

    my $left = 0;
    my $right = @disk.end;

    while $left < $right {
        while $left < $right and @disk[$left] ne '.' {
            $left++;
        }
        while $left < $right and @disk[$right] eq '.' {
            $right--;
        }

        if $left < $right {
            (@disk[$left], @disk[$right]) = (@disk[$right], @disk[$left]);
            $left++;
            $right--;
        }
    }

    my $checksum = [+] @disk.kv.map: -> $i, $val {
        $val ne '.' ?? $i * $val.Int !! 0
    };
    say $checksum;
}
