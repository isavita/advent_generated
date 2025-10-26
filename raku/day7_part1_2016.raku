
sub has-abba(Str $s) {
    for 0..$s.chars-4 -> $i {
        my \a = $s.substr($i,1);
        my \b = $s.substr($i+1,1);
        return True if a ne b && a eq $s.substr($i+3,1) && b eq $s.substr($i+2,1);
    }
    False
}

sub supports-tls(Str $ip) {
    my Bool $hypernet = False;
    my Bool $abba-outside = False;
    my Bool $abba-inside = False;

    for 0..$ip.chars-4 -> $i {
        if $ip.substr($i,1) eq '[' { $hypernet = True }
        elsif $ip.substr($i,1) eq ']' { $hypernet = False }
        elsif has-abba($ip.substr($i,4)) {
            $hypernet ?? ($abba-inside = True) !! ($abba-outside = True);
        }
    }
    $abba-outside && !$abba-inside
}

say slurp('input.txt').lines.grep({supports-tls($_)}).elems
