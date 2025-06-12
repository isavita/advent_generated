
/* REXX */
main:
    file_content = charin("input.txt", 1, chars("input.txt"))

    do forever
        p_red = pos(':"red"', file_content)
        if p_red = 0 then leave

        nest_level = 0
        start_pos = 0
        do i = p_red to 1 by -1
            char = substr(file_content, i, 1)
            if char == '}' then nest_level = nest_level + 1
            if char == '{' then nest_level = nest_level - 1
            if nest_level < 0 then do
                start_pos = i
                leave
            end
        end
        if start_pos = 0 then leave

        nest_level = 1
        end_pos = 0
        do i = start_pos + 1 to length(file_content)
            char = substr(file_content, i, 1)
            if char == '{' then nest_level = nest_level + 1
            if char == '}' then nest_level = nest_level - 1
            if nest_level = 0 then do
                end_pos = i
                leave
            end
        end
        if end_pos = 0 then leave

        prefix = substr(file_content, 1, start_pos - 1)
        suffix = substr(file_content, end_pos + 1)
        file_content = prefix || " " || suffix
    end

    bad_chars = '[]{},":abcdefghijklmnopqrstuvwxyz'
    numerics_only = translate(file_content, copies(' ', length(bad_chars)), bad_chars)

    total = 0
    do i = 1 to words(numerics_only)
        total = total + word(numerics_only, i)
    end

    say total
return
