sub find-position(@mat, $ch) {
    for ^@mat -> $i {
        for ^@mat[$i].chars -> $j {
            return ($i, $j) if @mat[$i].substr($j, 1) eq $ch;
        }
    }
    return (-1, -1);
}

sub ok(@mat, ($i, $j), $seq) {
    my ($ci, $cj) = ($i, $j);
    for $seq.comb -> $ch {
        return False if @mat[$ci].substr($cj, 1) eq ' ';
        given $ch {
            when '^' { $ci-- }
            when 'v' { $ci++ }
            when '<' { $cj-- }
            when '>' { $cj++ }
        }
        return False if $ci < 0 || $ci >= @mat.elems || $cj < 0 || $cj >= @mat[0].chars;
    }
    return True;
}

sub generate-moves(($i, $j), $objective, @pad) {
    my ($oi, $oj) = find-position(@pad, $objective);
    my $ret = '';
    if $j > $oj { $ret ~= '<' x ($j - $oj) }
    if $i > $oi { $ret ~= '^' x ($i - $oi) }
    if $i < $oi { $ret ~= 'v' x ($oi - $i) }
    if $j < $oj { $ret ~= '>' x ($oj - $j) }
    unless ok(@pad, ($i, $j), $ret) {
        $ret = '';
        if $j < $oj { $ret ~= '>' x ($oj - $j) }
        if $i > $oi { $ret ~= '^' x ($i - $oi) }
        if $i < $oi { $ret ~= 'v' x ($oi - $i) }
        if $j > $oj { $ret ~= '<' x ($j - $oj) }
    }
    return $ret;
}

sub solve($code, $robots, @key-pad, @robot-pad, $max-robots, %memo) {
    my $memo-key = "$code-$robots";
    return %memo{$memo-key} if %memo{$memo-key}:exists;
    return $code.chars if $robots <= 0;
    my ($pos-i, $pos-j) = $robots == $max-robots ?? (3, 2) !! (0, 2);
    my $ret = 0;
    for $code.comb -> $ch {
        my $moves;
        if $robots == $max-robots {
            $moves = generate-moves(($pos-i, $pos-j), $ch, @key-pad);
            ($pos-i, $pos-j) = find-position(@key-pad, $ch);
        } else {
            $moves = generate-moves(($pos-i, $pos-j), $ch, @robot-pad);
            ($pos-i, $pos-j) = find-position(@robot-pad, $ch);
        }
        $ret += solve($moves ~ 'A', $robots - 1, @key-pad, @robot-pad, $max-robots, %memo);
    }
    %memo{$memo-key} = $ret;
    return $ret;
}

sub MAIN() {
    my @key-pad = '789', '456', '123', ' 0A';
    my @robot-pad = ' ^A', '<v>';
    my $max-robots = 3;
    my $answer = 0;
    for 'input.txt'.IO.lines -> $line {
        my $trimmed = $line.trim;
        next if $trimmed eq '';
        my $numeric-part = +($trimmed.comb.grep(/\d/).join);
        my %memo;
        $answer += solve($trimmed, $max-robots, @key-pad, @robot-pad, $max-robots, %memo) * $numeric-part;
    }
    say $answer;
}