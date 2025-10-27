
sub MAIN {
    my $in = slurp 'input.txt';
    my @prog = 'a'..'p';
    for $in.split(',') {
        when /^s(\d+)/          { @prog.=rotate(-$0) }
        when /^x(\d+)\/(\d+)/   { @prog[$0,$1] = @prog[$1,$0] }
        when /^p(\w)\/(\w)/     { @prog = @prog.map: { $_ eq $0 ?? $1 !! $_ eq $1 ?? $0 !! $_ } }
    }
    put @prog.join
}
