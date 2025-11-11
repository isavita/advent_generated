
#!/usr/bin/env raku

my @key-pad = ["789", "456", "123", " 0A"];
my @robot-pad = [" ^A", "<v>"];
my $MAX-ROBOTS = 26;
my %memo;

sub find-position(@pad, $ch) {
    for @pad.kv -> $r, $row {
        with $row.index($ch) { return ($r, $_) }
    }
    (-1, -1)
}

sub is-ok(@pad, $r, $c, $seq) {
    my ($cur-r, $cur-c) = ($r, $c);
    for $seq.comb -> $move {
        return False if @pad[$cur-r].substr($cur-c, 1) eq ' ';
        given $move {
            when '^' { $cur-r-- }
            when 'v' { $cur-r++ }
            when '<' { $cur-c-- }
            when '>' { $cur-c++ }
        }
        return False if $cur-r < 0 || $cur-r >= @pad.elems;
        return False if $cur-c < 0 || $cur-c >= @pad[0].chars;
    }
    @pad[$cur-r].substr($cur-c, 1) ne ' '
}

sub generate-moves($r, $c, $target, @pad) {
    my ($target-r, $target-c) = find-position(@pad, $target);
    my $moves = "";
    $moves ~= '<' x ($c - $target-c) if $c > $target-c;
    $moves ~= '^' x ($r - $target-r) if $r > $target-r;
    $moves ~= 'v' x ($target-r - $r) if $r < $target-r;
    $moves ~= '>' x ($target-c - $c) if $c < $target-c;
    return $moves if is-ok(@pad, $r, $c, $moves);
    $moves = "";
    $moves ~= '>' x ($target-c - $c) if $c < $target-c;
    $moves ~= '^' x ($r - $target-r) if $r > $target-r;
    $moves ~= 'v' x ($target-r - $r) if $r < $target-r;
    $moves ~= '<' x ($c - $target-c) if $c > $target-c;
    $moves
}

sub solve($code, $robots) {
    my $key = "$code|$robots";
    return %memo{$key} if %memo{$key}:exists;
    if $robots <= 0 {
        %memo{$key} = $code.chars;
        return $code.chars;
    }
    my ($cur-r, $cur-c) = $robots == $MAX-ROBOTS ?? (3, 2) !! (0, 2);
    my @pad = $robots == $MAX-ROBOTS ?? @key-pad !! @robot-pad;
    my $total = 0;
    for $code.comb -> $ch {
        my $moves = generate-moves($cur-r, $cur-c, $ch, @pad);
        ($cur-r, $cur-c) = find-position(@pad, $ch);
        $total += solve($moves ~ "A", $robots - 1);
    }
    %memo{$key} = $total
}

sub MAIN() {
    my $file = "input.txt";
    die "File '$file' not found" unless $file.IO.e;
    my $result = 0;
    for $file.IO.lines.map(*.trim).grep(*.so) -> $line {
        if $line ~~ m/(\d+)/ {
            my $num = $0.Int;
            $result += solve($line, $MAX-ROBOTS) * $num;
        }
    }
    say $result;
}
