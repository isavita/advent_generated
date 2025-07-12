
function solve()
    % Read input file
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    lines = lines{1};
    fclose(fid);

    % Initialize tracks and carts
    tracks = cell(size(lines));
    carts = struct('x', {}, 'y', {}, 'direction', {}, 'turns', {});

    for y = 0:numel(lines)-1
        currentLine = lines{y+1};
        tracks{y+1} = currentLine;

        for x = 0:numel(currentLine)-1
            char_at_pos = currentLine(x+1);
            if ismember(char_at_pos, ['>', '<', '^', 'v'])
                newCart.x = x;
                newCart.y = y;
                newCart.direction = char_at_pos;
                newCart.turns = 0;
                carts(end+1) = newCart; %#ok<AGROW>

                if char_at_pos == '>' || char_at_pos == '<'
                    tracks{y+1}(x+1) = '-';
                else
                    tracks{y+1}(x+1) = '|';
                end
            end
        end
    end

    % Simulation loop
    while numel(carts) > 1
        % Sort carts by y then x
        cart_coords = zeros(numel(carts), 2);
        for k = 1:numel(carts)
            cart_coords(k, 1) = carts(k).y;
            cart_coords(k, 2) = carts(k).x;
        end
        [~, sort_idx] = sortrows(cart_coords);
        carts = carts(sort_idx);

        toRemove = false(1, numel(carts));

        for i = 1:numel(carts)
            if toRemove(i)
                continue;
            end

            carts(i) = moveCart(carts(i), tracks);

            crash_idx = checkCrash(carts(i), carts, i);

            if crash_idx ~= 0
                toRemove(i) = true;
                toRemove(crash_idx) = true;
            end
        end

        carts = carts(~toRemove);
    end

    fprintf('%d,%d\n', carts(1).x, carts(1).y);

end

function cart = moveCart(cart, tracks)
    x = cart.x;
    y = cart.y;
    dir = cart.direction;
    turns = cart.turns;

    switch dir
        case '>'
            x = x + 1;
        case '<'
            x = x - 1;
        case '^'
            y = y - 1;
        case 'v'
            y = y + 1;
    end

    track_char = tracks{y+1}(x+1);

    switch track_char
        case '+'
            if mod(turns, 3) == 0
                dir = turnLeft(dir);
            elseif mod(turns, 3) == 2
                dir = turnRight(dir);
            end
            turns = turns + 1;
        case '/'
            dir = turnSlash(dir);
        case '\'
            dir = turnBackslash(dir);
    end

    cart.x = x;
    cart.y = y;
    cart.direction = dir;
    cart.turns = turns;
end

function crash_idx = checkCrash(currentCart, allCarts, currentIdx)
    crash_idx = 0;
    for i = 1:numel(allCarts)
        if i == currentIdx
            continue;
        end
        if allCarts(i).x == currentCart.x && allCarts(i).y == currentCart.y
            crash_idx = i;
            return;
        end
    end
end

function new_dir = turnLeft(current_dir)
    switch current_dir
        case '>'
            new_dir = '^';
        case '<'
            new_dir = 'v';
        case '^'
            new_dir = '<';
        case 'v'
            new_dir = '>';
    end
end

function new_dir = turnRight(current_dir)
    switch current_dir
        case '>'
            new_dir = 'v';
        case '<'
            new_dir = '^';
        case '^'
            new_dir = '>';
        case 'v'
            new_dir = '<';
    end
end

function new_dir = turnSlash(current_dir)
    switch current_dir
        case '>'
            new_dir = '^';
        case '<'
            new_dir = 'v';
        case '^'
            new_dir = '>';
        case 'v'
            new_dir = '<';
    end
end

function new_dir = turnBackslash(current_dir)
    switch current_dir
        case '>'
            new_dir = 'v';
        case '<'
            new_dir = '^';
        case '^'
            new_dir = '<';
        case 'v'
            new_dir = '>';
    end
end
