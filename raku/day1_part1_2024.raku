
sub MAIN {
    my @a = 'input.txt'.IO.lines>>.words>>.[0]>>.Int.sort;
    my @b = 'input.txt'.IO.lines>>.words>>.[1]>>.Int.sort;
    say [+] (@a Z- @b)».abs
}
