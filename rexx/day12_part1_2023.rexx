
call main
exit

main:
    total = 0
    file = 'input.txt'
    do while lines(file) > 0
        line = linein(file)
        parse var line s ' ' gs
        if s = '' then iterate

        drop g.
        g.0 = 0
        gs = translate(gs, " ", ",")
        do i = 1 to words(gs)
            g.i = word(gs, i)
            g.0 = i
        end

        sl = length(s)
        drop cache.
        total = total + count(1, 1, 0)
    end
    say total
return

count:
    procedure expose s sl g. cache.
    arg s_idx, g_idx, c_dmg

    key = s_idx'.'g_idx'.'c_dmg
    if symbol('cache.'key) == 'VAR' then return cache.key

    if s_idx > sl then do
        if g_idx > g.0 & c_dmg = 0 then return 1
        if g_idx = g.0 & c_dmg = g.g_idx then return 1
        return 0
    end

    res = 0
    char = substr(s, s_idx, 1)

    if char <> '#' then do
        if c_dmg = 0 then
            res = res + count(s_idx + 1, g_idx, 0)
        else if g_idx <= g.0 & c_dmg = g.g_idx then
            res = res + count(s_idx + 1, g_idx + 1, 0)
    end

    if char <> '.' then do
        if g_idx <= g.0 & c_dmg < g.g_idx then
            res = res + count(s_idx + 1, g_idx, c_dmg + 1)
    end

    cache.key = res
    return res
