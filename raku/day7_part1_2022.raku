
class Dir {
    has $.name;
    has $.parent;
    has %!files;
    has @!subdirs;
    method files   { %!files }
    method subdirs { @!subdirs }
    method add-dir(Dir $d) { @!subdirs.push: $d }
    method add-file(Str $name, Int $size) { %!files{$name} = $size }
    method total-size {
        %!files.values.sum + @!subdirs.map({ .total-size }).sum
    }
}

sub MAIN() {
    my $root = Dir.new(name => '/', parent => Nil);
    my $cur  = $root;

    for slurp('input.txt').lines -> $line {
        if $line.starts-with('$') {
            if $line ~~ /cd \s+ (\S+)/ {
                my $dir = $0;
                given $dir {
                    when '..' { $cur = $cur.parent // $cur }
                    when '/'  { $cur = $root }
                    default   { $cur = $cur.subdirs.first({ .name eq $dir }) // $cur }
                }
            }
        } else {
            if $line ~~ /dir \s+ (\S+)/ {
                my $dname = $0;
                $cur.add-dir( Dir.new(name => $dname, parent => $cur) );
            } else {
                my ($size, $fname) = $line.split(' ', 2);
                $cur.add-file($fname, +$size);
            }
        }
    }

    my @stack = $root;
    my @dirs;
    while @stack {
        my $d = @stack.pop;
        @dirs.push($d);
        @stack.append: $d.subdirs;
    }

    my $sum = 0;
    for @dirs -> $d {
        my $sz = $d.total-size;
        $sum += $sz if $sz <= 100_000;
    }

    say "Sum of total sizes of directories with size at most 100000: $sum";
}
