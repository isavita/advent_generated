
sub MAIN {
    my %dirs = '' => 0;
    my @curr = '';

    for "input.txt".IO.lines -> $line {
        my @parts = $line.split(' ');
        if @parts[0] eq '$' && @parts[1] eq 'cd' {
            given @parts[2] {
                when '/'   { @curr = ('') }
                when '..'  { @curr.pop if @curr.elems > 1 }
                default     { @curr.append(@parts[2]) }
            }
        }
        elsif @parts[0] ne 'dir' && @parts[0] !~~ /^\$/ {
            my $size = +@parts[0];
            for 0 .. @curr.elems-1 -> $i {
                my $path = @curr[0..$i].join('/');
                %dirs{$path} += $size;
            }
        }
    }

    my $total = 70_000_000;
    my $want  = 30_000_000;
    my $available = $total - %dirs{''};
    my $need = $want - $available;

    say %dirs.values.grep(* >= $need).min;
}
