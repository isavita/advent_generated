
sub has-abba(Str $s) {
    for ^($s.chars - 3) -> $i {
        my \a = $s.substr($i, 1);
        my \b = $s.substr($i + 1, 1);
        return True if a eq $s.substr($i + 3, 1) and b eq $s.substr($i + 2, 1) and a ne b;
    }
    False
}

sub supports-tls(Str $ip) {
    my Bool $hypernet = False;
    my Bool $abba-outside = False;
    my Bool $abba-inside  = False;
    for ^($ip.chars - 3) -> $i {
        given $ip.substr($i, 1) {
            when '[' { $hypernet = True }
            when ']' { $hypernet = False }
            default {
                if has-abba($ip.substr($i, 4)) {
                    $hypernet ?? ($abba-inside  = True)
                              !! ($abba-outside = True);
                }
            }
        }
    }
    $abba-outside && !$abba-inside
}

sub has-aba(Str $s) {
    gather for ^($s.chars - 2) -> $i {
        my \a = $s.substr($i, 1);
        my \b = $s.substr($i + 1, 1);
        take $s.substr($i, 3) if a eq $s.substr($i + 2, 1) and a ne b;
    }
}

sub supports-ssl(Str $ip) {
    my Str @hypernet-abas;
    my Str @supernet-abas;
    my Bool $hypernet = False;
    for ^($ip.chars - 2) -> $i {
        given $ip.substr($i, 1) {
            when '[' { $hypernet = True }
            when ']' { $hypernet = False }
            default {
                if $hypernet {
                    @hypernet-abas.append: has-aba($ip.substr($i, 3));
                } else {
                    @supernet-abas.append: has-aba($ip.substr($i, 3));
                }
            }
        }
    }
    for @supernet-abas -> $aba {
        my Str $bab = $aba.substr(1,1) ~ $aba.substr(0,1) ~ $aba.substr(1,1);
        return True if $bab ∈ @hypernet-abas;
    }
    False
}

my Str @data = 'input.txt'.IO.lines;
my Int $tls-count = 0;
my Int $ssl-count = 0;

for @data -> $ip {
    ++$tls-count if supports-tls($ip);
    ++$ssl-count if supports-ssl($ip);
}

say $tls-count;
say $ssl-count;
