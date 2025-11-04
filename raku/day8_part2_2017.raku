
sub MAIN {
    my %h; my $top = -∞;
    for 'input.txt'.IO.lines {
        my ($r,$op,$v,'if',$c,$cop,$cv) = .words;
        %h{$c} //= 0;
        my $ok = do given $cop {
            when '>'  { %h{$c} >  $cv }
            when '>=' { %h{$c} >= $cv }
            when '<'  { %h{$c} <  $cv }
            when '<=' { %h{$c} <= $cv }
            when '==' { %h{$c} == $cv }
            when '!=' { %h{$c} != $cv }
        }
        if $ok {
            %h{$r} //= 0;
            %h{$r} += ($op eq 'inc' ?? 1 !! -1) * $v;
            $top max= %h{$r};
        }
    }
    put $top;
}
