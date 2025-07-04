
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%s');
    fclose(fid);
    data = data{1};

    gamma_rate = calculate_rate(data, true);
    epsilon_rate = calculate_rate(data, false);

    power_consumption = gamma_rate * epsilon_rate;
    fprintf('%d\n', power_consumption);
end

function rate = calculate_rate(numbers, common)
    num_bits = length(numbers{1});
    bit_counts = zeros(num_bits, 2);

    for i = 1:length(numbers)
        for j = 1:num_bits
            if numbers{i}(j) == '1'
                bit_counts(j, 2) = bit_counts(j, 2) + 1;
            else
                bit_counts(j, 1) = bit_counts(j, 1) + 1;
            end
        end
    end

    rate_str = '';
    for j = 1:num_bits
        if common
            if bit_counts(j, 2) > bit_counts(j, 1)
                rate_str = [rate_str, '1'];
            else
                rate_str = [rate_str, '0'];
            end
        else
            if bit_counts(j, 2) < bit_counts(j, 1)
                rate_str = [rate_str, '1'];
            else
                rate_str = [rate_str, '0'];
            end
        end
    end
    rate = bin2dec(rate_str);
end

main();
