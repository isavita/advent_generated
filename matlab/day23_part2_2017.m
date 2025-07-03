
function main()
    fid = fopen('input.txt', 'r');
    fclose(fid);

    b = 57 * 100 + 100000;
    c = b + 17000;
    h = 0;

    for x = b:17:c
        if ~isprime(x)
            h = h + 1;
        end
    end

    disp(h);
end

function result = isprime(n)
    if n < 2
        result = false;
        return;
    end
    for i = 2:sqrt(n)
        if mod(n, i) == 0
            result = false;
            return;
        end
    end
    result = true;
end
