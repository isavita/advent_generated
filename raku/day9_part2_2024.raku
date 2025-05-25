
sub MAIN {
    my $line = slurp 'input.txt'.trim;

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

    my @files;
    my $cur-id = -1;
    my $start;
    for @disk.kv -> $i, $b {
        if $b eq '.' {
            $cur-id = -1;
            next;
        }
        my $id = $b.Int;
        if $id != $cur-id {
            $cur-id = $id;
            $start = $i;
        }
        if $i == @disk.end || @disk[$i + 1] ne $b {
            @files.push: {:id($id), :start($start), :end($i)};
        }
    }

    for @files.reverse -> $f {
        my $file-len = $f<end> - $f<start> + 1;
        my $leftmost-span = -1;
        my $span-len = 0;
        for 0 ..^ $f<start> -> $j {
            if @disk[$j] eq '.' {
                if $span-len == 0 {
                    $leftmost-span = $j;
                }
                $span-len++;
                if $span-len == $file-len {
                    last;
                }
            } else {
                $span-len = 0;
                $leftmost-span = -1;
            }
        }
        if $leftmost-span != -1 && $span-len == $file-len {
            for $f<start> .. $f<end> -> $x {
                @disk[$x] = '.';
            }
            for 0 ..^ $file-len -> $x {
                @disk[$leftmost-span + $x] = $f<id>.Str;
            }
        }
    }

    my $checksum = 0;
    for @disk.kv -> $i, $b {
        if $b ne '.' {
            $checksum += $i * $b.Int;
        }
    }
    say $checksum;
}
