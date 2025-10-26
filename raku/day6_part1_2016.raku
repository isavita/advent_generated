
sub MAIN {
    my @lines = 'input.txt'.IO.lines;
    my $message = (0 .. @lines[0].chars-1).map(-> $i { 
        @lines.map( *.substr($i,1) ).Bag.max(*.value).key 
    }).join;
    put $message;
}
