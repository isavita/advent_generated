
using Printf

# ---------- helpers ----------
pow10(n::Int) = big(10)^n               # 10ⁿ as BigInt
digitlen(x::BigInt) = length(string(x))

# ---------- read input ----------
txt = read("input.txt", String)
txt = replace(txt, r"\r?\n" => "")
txt = strip(txt)
isempty(txt) && exit()

parts = filter(!isempty, split(txt, ','))
ranges = map(parts) do p
    a, b = split(p, '-')
    (big(parse(Int, a)), big(parse(Int, b)))   # (s, e) as BigInt
end

found = Set{BigInt}()

# ---------- main algorithm ----------
for (s, e) in ranges
    s_len = digitlen(s)
    e_len = digitlen(e)

    for total_len in s_len:e_len
        max_k = total_len ÷ 2
        for k in 1:max_k
            total_len % k != 0 && continue
            reps = total_len ÷ k

            # M = Σ_{i=0}^{reps-1} 10^{i·k} = (10^{k·reps}‑1)/(10^{k}‑1)
            ten_k   = pow10(k)
            ten_kr  = pow10(k * reps)
            M = (ten_kr - 1) ÷ (ten_k - 1)

            min_seed = pow10(k - 1)
            max_seed = ten_k - 1

            target_min = (s + M - 1) ÷ M          # ceil(s / M)
            target_max = e ÷ M                    # floor(e / M)

            start = max(target_min, min_seed)
            stop  = min(target_max, max_seed)
            start > stop && continue

            for cur in start:stop
                push!(found, cur * M)
            end
        end
    end
end

# ---------- output ----------
total = sum(found)
@printf "Sum of invalid IDs: %s\n" total
