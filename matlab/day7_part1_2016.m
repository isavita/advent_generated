
function main()
    fid = fopen('input.txt', 'r');
    ips = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    ips = ips{1};

    count = 0;
    for i = 1:length(ips)
        if supports_tls(ips{i})
            count = count + 1;
        end
    end
    disp(count);
end

function result = supports_tls(ip)
    hypernet = false;
    abba_outside = false;
    abba_inside = false;

    for i = 1:length(ip)
        if ip(i) == '['
            hypernet = true;
        elseif ip(i) == ']'
            hypernet = false;
        elseif i <= length(ip) - 3
            if has_abba(ip(i:i+3))
                if hypernet
                    abba_inside = true;
                else
                    abba_outside = true;
                end
            end
        end
    end
    result = abba_outside && ~abba_inside;
end

function result = has_abba(s)
    result = false;
    if length(s) == 4 && s(1) ~= s(2) && s(1) == s(4) && s(2) == s(3)
        result = true;
    end
end

main();
