
/* REXX */

main:
    adj. = ''
    visited. = 0
    groups = 0
    max_node = 0
    fname = 'input.txt'

    do while lines(fname) > 0
        line = linein(fname)
        parse var line from_node ' <-> ' to_list
        max_node = max(max_node, from_node)
        do while to_list <> ''
            parse var to_list to_node ',' to_list
            to_node = strip(to_node)
            max_node = max(max_node, to_node)
            adj.from_node = strip(adj.from_node to_node)
            adj.to_node = strip(adj.to_node from_node)
        end
    end

    do node = 0 to max_node
        if visited.node = 0 & adj.node <> '' then do
            call DFS node
            groups = groups + 1
        end
    end

    say groups
exit

DFS:
    procedure expose adj. visited.
    parse arg node
    visited.node = 1
    do i = 1 to words(adj.node)
        neighbor = word(adj.node, i)
        if visited.neighbor = 0 then
            call DFS neighbor
    end
return
