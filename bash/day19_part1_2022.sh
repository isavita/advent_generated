
#!/usr/bin/env bash
python3 - <<'PY'
import sys, re
from functools import lru_cache

blueprints = []
with open('input.txt') as f:
    for line in f:
        m = re.match(r'Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.', line.strip())
        if m:
            blueprints.append({
                'id': int(m.group(1)),
                'ore_ore': int(m.group(2)),
                'ore_clay': int(m.group(3)),
                'obs_ore': int(m.group(4)),
                'obs_clay': int(m.group(5)),
                'geo_ore': int(m.group(6)),
                'geo_obs': int(m.group(7)),
            })

def max_geodes(bp, total_time):
    @lru_cache(maxsize=None)
    def dfs(time, ore, clay, obs, geo, ore_r, clay_r, obs_r, geo_r, earliest_geo):
        if time == total_time:
            return geo
        # prune if no geodes yet and passed earliest possible
        if geo == 0 and time > earliest_geo:
            return 0
        best = geo
        # try building geode robot
        if ore >= bp['geo_ore'] and obs >= bp['geo_obs']:
            best = max(best, dfs(
                time+1,
                ore - bp['geo_ore'] + ore_r,
                clay + clay_r,
                obs - bp['geo_obs'] + obs_r,
                geo + geo_r,
                ore_r,
                clay_r,
                obs_r,
                geo_r+1,
                min(earliest_geo, time+1) if geo_r==0 else earliest_geo
            ))
            return best
        # else consider other builds
        # ore robot
        if time <= total_time - 16 and ore_r < bp['obs_ore']*2 and ore >= bp['ore_ore']:
            best = max(best, dfs(
                time+1,
                ore - bp['ore_ore'] + ore_r,
                clay + clay_r,
                obs + obs_r,
                geo + geo_r,
                ore_r+1,
                clay_r,
                obs_r,
                geo_r,
                earliest_geo
            ))
        # clay robot
        if time <= total_time - 8 and clay_r < bp['obs_clay'] and ore >= bp['ore_clay']:
            best = max(best, dfs(
                time+1,
                ore - bp['ore_clay'] + ore_r,
                clay + clay_r,
                obs + obs_r,
                geo + geo_r,
                ore_r,
                clay_r+1,
                obs_r,
                geo_r,
                earliest_geo
            ))
        # obsidian robot
        if time <= total_time - 4 and obs_r < bp['geo_obs'] and ore >= bp['obs_ore'] and clay >= bp['obs_clay']:
            best = max(best, dfs(
                time+1,
                ore - bp['obs_ore'] + ore_r,
                clay - bp['obs_clay'] + clay_r,
                obs + obs_r,
                geo + geo_r,
                ore_r,
                clay_r,
                obs_r+1,
                geo_r,
                earliest_geo
            ))
        # wait
        best = max(best, dfs(
            time+1,
            ore + ore_r,
            clay + clay_r,
            obs + obs_r,
            geo + geo_r,
            ore_r,
            clay_r,
            obs_r,
            geo_r,
            earliest_geo
        ))
        return best

    return dfs(0,0,0,0,0,1,0,0,0,total_time)

total = 0
for bp in blueprints:
    total += bp['id'] * max_geodes(bp, 24)
print(total)
PY
