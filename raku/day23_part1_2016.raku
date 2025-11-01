
my @prog = 'input.txt'.IO.lines.map: -> $l {
    my @w = $l.split(' ');
    @w[1] = +@w[1] if @w[1] ~~ /^\-?\d+$/;
    @w[2] = +@w[2] if @w.elems > 1 && @w[2] ~~ /^\-?\d+$/;
    @w
}

my %r = a => 7, b => 0, c => 0, d => 0;

sub val($x) { $x ~~ /\d/ ?? $x !! %r{$x} }

my int $ip = 0;
while 0 <= $ip < @prog {
    my @i := @prog[$ip];
    given @i[0] {
        when 'cpy' { %r{@i[2]} = val(@i[1]) if @i[2] ~~ /<[a..d]>/ }
        when 'inc' { %r{@i[1]}++ if @i[1] ~~ /<[a..d]>/ }
        when 'dec' { %r{@i[1]}-- if @i[1] ~~ /<[a..d]>/ }
        when 'jnz' { $ip += (val(@i[1]) ?? val(@i[2]) - 1 !! 0) }
        when 'tgl' {
            my int $t = $ip + val(@i[1]);
            if 0 <= $t < @prog {
                @prog[$t][0] = @prog[$t][0] eq 'inc'  ?? 'dec' !!
                               @prog[$t][0] eq 'dec' ?? 'inc' !!
                               @prog[$t][0] eq 'jnz' ?? 'cpy' !!
                               @prog[$t][0] eq 'cpy' ?? 'jnz' !!
                               @prog[$t][0] eq 'tgl' ?? 'inc' !! @prog[$t][0];
            }
        }
    }
    $ip++;
}

say %r<a>;
