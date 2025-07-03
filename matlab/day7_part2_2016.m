
function main()
    fileId = fopen('input.txt', 'r');
    data = textscan(fileId, '%s', 'Delimiter', '\n');
    fclose(fileId);
    data = data{1};

    tls_count = 0;
    ssl_count = 0;

    for i = 1:length(data)
        ip = data{i};
        if supports_tls(ip)
            tls_count = tls_count + 1;
        end
        if supports_ssl(ip)
            ssl_count = ssl_count + 1;
        end
    end

    fprintf('%d\n', tls_count);
    fprintf('%d\n', ssl_count);
end

function result = has_abba(s)
    result = false;
    for i = 1:(length(s) - 3)
        if s(i) == s(i+3) && s(i+1) == s(i+2) && s(i) ~= s(i+1)
            result = true;
            return;
        end
    end
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

function aba_list = has_aba(s)
    aba_list = {};
    for i = 1:(length(s) - 2)
        if s(i) == s(i+2) && s(i) ~= s(i+1)
            aba_list{end+1} = s(i:i+2);
        end
    end
end

function result = supports_ssl(ip)
    hypernet_abas = {};
    supernet_abas = {};
    hypernet = false;
    for i = 1:length(ip)
        if ip(i) == '['
            hypernet = true;
        elseif ip(i) == ']'
            hypernet = false;
        elseif i <= length(ip) - 2
            if hypernet
                current_abas = has_aba(ip(i:i+2));
                hypernet_abas = [hypernet_abas, current_abas];
            else
                current_abas = has_aba(ip(i:i+2));
                supernet_abas = [supernet_abas, current_abas];
            end
        end
    end
    
    for i = 1:length(supernet_abas)
        aba = supernet_abas{i};
        bab = [aba(2), aba(1), aba(2)];
        for j = 1:length(hypernet_abas)
            if strcmp(bab, hypernet_abas{j})
                result = true;
                return;
            end
        end
    end
    result = false;
end

main();
