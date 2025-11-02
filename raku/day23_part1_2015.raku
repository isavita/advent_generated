
#!/usr/bin/env raku
sub MAIN() {
    my @inst = "input.txt".IO.lines.map: *.trim;
    my $a = 0;
    my $b = 0;
    my $pc = 0;
    while $pc >= 0 && $pc < @inst.elems {
        my ($op, $arg1, $arg2) = @inst[$pc].split(' ', *, :skip-empty);
        my $offset = 1;
        given $op {
            when 'hlf'  { $a = $a div 2 if $arg1 eq 'a'; $b = $b div 2 if $arg1 eq 'b' }
            when 'tpl'  { $a = $a * 3  if $arg1 eq 'a'; $b = $b * 3  if $arg1 eq 'b' }
            when 'inc'  { $a++          if $arg1 eq 'a'; $b++          if $arg1 eq 'b' }
            when 'jmp'  { $offset = $arg1.Int }
            when 'jie'  {
                my $reg = $arg1.substr(0,1);
                $offset = $arg2.Int if $reg eq 'a' ?? $a % 2 == 0 !! $b % 2 == 0
            }
            when 'jio'  {
                my $reg = $arg1.substr(0,1);
                $offset = $arg2.Int if $reg eq 'a' ?? $a == 1 !! $b == 1
            }
        }
        $pc += $offset;
    }
    say $b;
}
