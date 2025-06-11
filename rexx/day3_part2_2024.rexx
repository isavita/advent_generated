
/* REXX */
parse arg input_file .
if input_file = '' then input_file = 'input.txt'

rc = stream(input_file, 'c', 'open read')
if rc \= 'READY:' then do
    say 'Error opening file: ' || input_file || ' - ' || rc
    exit 1
end

program_text = ''
do while lines(input_file)
    program_text = program_text || linein(input_file)
end
call stream input_file , 'c' , 'close'

enabled = 1
totalSum = 0
current_pos = 1
text_len = length(program_text)

do while current_pos <= text_len
    found_match = 0

    if current_pos + 3 <= text_len then do
        if substr(program_text, current_pos, 4) = 'mul(' then do
            mul_start_val = current_pos + 4
            mul_end_val = pos(')', program_text, mul_start_val)
            if mul_end_val > 0 then do
                mul_str_content = substr(program_text, mul_start_val, mul_end_val - mul_start_val)
                comma_pos = pos(',', mul_str_content)
                if comma_pos > 0 then do
                    x_str = substr(mul_str_content, 1, comma_pos - 1)
                    y_str = substr(mul_str_content, comma_pos + 1)
                    if datatype(x_str, 'N') & datatype(y_str, 'N') then do
                        if (length(x_str) >= 1 & length(x_str) <= 3) & (length(y_str) >= 1 & length(y_str) <= 3) then do
                            if enabled then
                                totalSum = totalSum + (x_str * y_str)
                            current_pos = mul_end_val + 1
                            found_match = 1
                        end
                    end
                end
            end
        end
    end

    if found_match = 0 then do
        if current_pos + 3 <= text_len then do
            if substr(program_text, current_pos, 4) = 'do()' then do
                enabled = 1
                current_pos = current_pos + 4
                found_match = 1
            end
        end
    end

    if found_match = 0 then do
        if current_pos + 6 <= text_len then do
            if substr(program_text, current_pos, 7) = "don't()" then do
                enabled = 0
                current_pos = current_pos + 7
                found_match = 1
            end
        end
    end

    if found_match = 0 then
        current_pos = current_pos + 1
end

say totalSum
exit 0
