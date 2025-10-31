use v6;

sub MAIN() {
    my $current = "input.txt".IO.slurp.chomp;
    my $first = next-password($current);
    my $second = next-password($first);
    say $second;
}

sub next-password($pw) {
    my $new = $pw;
    repeat {
        $new = increment($new);
    } until is-valid($new);
    $new;
}

sub increment($pw) {
    my @c = $pw.comb;
    my $i = @c.elems - 1;
    while $i >= 0 {
        my $val = @c[$i].ord;
        $val++;
        if $val > 'z'.ord {
            @c[$i] = 'a';
            $i--;
        } else {
            @c[$i] = $val.chr;
            last;
        }
    }
    @c.join;
}

sub is-valid($pw) {
    has-straight($pw) && !contains-invalid($pw) && has-two-pairs($pw);
}

sub has-straight($pw) {
    my @c = $pw.comb;
    loop (my $i = 0; $i + 2 < @c.elems; $i++) {
        if @c[$i].ord + 1 == @c[$i+1].ord && @c[$i].ord + 2 == @c[$i+2].ord {
            return True;
        }
    }
    False;
}

sub contains-invalid($pw) {
    so $pw ~~ /<[i o l]>/;
}

sub has-two-pairs($pw) {
    my $count = 0;
    my @c = $pw.comb;
    loop (my $i = 0; $i + 1 < @c.elems; $i++) {
        if @c[$i] eq @c[$i+1] {
            $count++;
            $i++;
        }
    }
    $count >= 2;
}