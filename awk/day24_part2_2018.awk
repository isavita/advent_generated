
function battle(boost,    u, d, i, j, k, t, atk, def, best_t, max_dmg, cur_dmg, ep_def, ep_best, changed, side1_cnt, side2_cnt, idx, target, is_targeted, ep_i, ep_j, dmg_val, killed) {
    for (i=1; i<=N; i++) {
        u[i] = U[i]
        d[i] = D[i] + (S[i] == 1 ? boost : 0)
    }
    while (1) {
        side1_cnt = side2_cnt = 0
        for (i=1; i<=N; i++) {
            if (u[i] > 0) {
                if (S[i] == 1) side1_cnt += u[i]
                else side2_cnt += u[i]
            }
        }
        if (side1_cnt == 0 || side2_cnt == 0) break
        for (i=1; i<=N; i++) {
            idx[i] = i
            target[i] = 0
            is_targeted[i] = 0
        }
        for (i=1; i<N; i++) {
            for (j=i+1; j<=N; j++) {
                ep_i = u[idx[i]] * d[idx[i]]
                ep_j = u[idx[j]] * d[idx[j]]
                if (ep_j > ep_i || (ep_j == ep_i && I[idx[j]] > I[idx[i]])) {
                    t = idx[i]; idx[i] = idx[j]; idx[j] = t
                }
            }
        }
        for (i=1; i<=N; i++) {
            atk = idx[i]
            if (u[atk] <= 0) continue
            best_t = 0
            max_dmg = 0
            for (def=1; def<=N; def++) {
                if (u[def] <= 0 || S[atk] == S[def] || is_targeted[def]) continue
                cur_dmg = u[atk] * d[atk]
                if (imm[def, T[atk]]) cur_dmg = 0
                else if (wek[def, T[atk]]) cur_dmg *= 2
                if (cur_dmg == 0) continue
                if (cur_dmg > max_dmg) {
                    max_dmg = cur_dmg; best_t = def
                } else if (cur_dmg == max_dmg) {
                    ep_def = u[def] * d[def]
                    ep_best = u[best_t] * d[best_t]
                    if (ep_def > ep_best || (ep_def == ep_best && I[def] > I[best_t])) {
                        best_t = def
                    }
                }
            }
            if (best_t) {
                target[atk] = best_t
                is_targeted[best_t] = 1
            }
        }
        for (i=1; i<=N; i++) idx[i] = i
        for (i=1; i<N; i++) {
            for (j=i+1; j<=N; j++) {
                if (I[idx[j]] > I[idx[i]]) {
                    t = idx[i]; idx[i] = idx[j]; idx[j] = t
                }
            }
        }
        changed = 0
        for (i=1; i<=N; i++) {
            atk = idx[i]
            def = target[atk]
            if (u[atk] > 0 && def && u[def] > 0) {
                dmg_val = u[atk] * d[atk]
                if (imm[def, T[atk]]) dmg_val = 0
                else if (wek[def, T[atk]]) dmg_val *= 2
                killed = int(dmg_val / H[def])
                if (killed > u[def]) killed = u[def]
                if (killed > 0) {
                    u[def] -= killed
                    changed = 1
                }
            }
        }
        if (!changed) return -1
    }
    return (side1_cnt > 0) ? side1_cnt : -2
}

BEGIN {
    while ((getline < "input.txt") > 0) {
        if (/Immune System/) { s = 1; continue }
        if (/Infection/) { s = 2; continue }
        if ($0 == "") continue
        N++
        S[N] = s
        U[N] = $1
        H[N] = $5
        I[N] = $NF
        D[N] = $(NF-5)
        T[N] = $(NF-4)
        if (/\(/) {
            match($0, /\(.*\)/)
            p = substr($0, RSTART+1, RLENGTH-2)
            m = split(p, parts, "; ")
            for (k=1; k<=m; k++) {
                if (parts[k] ~ /^immune to/) {
                    sub(/^immune to /, "", parts[k])
                    ni = split(parts[k], items, ", ")
                    for (l=1; l<=ni; l++) imm[N, items[l]] = 1
                } else {
                    sub(/^weak to /, "", parts[k])
                    ni = split(parts[k], items, ", ")
                    for (l=1; l<=ni; l++) wek[N, items[l]] = 1
                }
            }
        }
    }
    for (boost=0; ; boost++) {
        res = battle(boost)
        if (res >= 0) {
            print res
            exit
        }
    }
}
