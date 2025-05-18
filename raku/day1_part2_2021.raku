
sub MAIN {
    my @depths = 'input.txt'.IO.lines.map(*.Int);
    say (@depths[1..*] Z> @depths[0..*-2]).sum;
    say (@depths[3..*] Z> @depths[0..*-4]).sum;
}
