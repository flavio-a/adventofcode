-- Add the directory containing this script to package.path
-- NOTE: this works ONLY if the script is run from cmd, not if it's required
do
    -- Path is the path to the directory containing this script. If this is
    -- run from within the same directory it is empty, hence the check
    local path = arg[0]:match("(.-)[^/]+$")
    if path ~= "" then
        package.path = table.concat{ package.path, ";", path, "/?.lua" }
    end
end

local tab = require('Wikilib-tables')
local str = require('Wikilib-strings')

local filename = str.trim(arg[1])

-- ============================================================================
-- ========================= ACTUAL CODE ==============================
local MEMOIZE = {}

-- Given a number of days n, compute the number of lanternfish that there are
-- after n days starting from a single lanternfish with internal timer 0
local function lanternfish(n)
    if n <= 0 then
        -- < 0 means a fish spawned near the end of days, that hence reaches
        -- day 0 without ever spwaning another
        return 1;
    end
    if MEMOIZE[n] then
        return MEMOIZE[n]
    else
        local cusu = 1
        -- After 1, 8, 15, ... days it spawns another fish with timer 8
        for i=1,n,7 do
            -- // n - i is the number of days that fish is alive
            -- // 8 is its initial timer
            -- // hence it spawns (n - i) - 8 fishes (including itself)
            cusu = cusu + lanternfish(n - i - 8)
        end
        MEMOIZE[n] = cusu
        return cusu
    end
end

local fishes
for line in io.lines(filename) do
    if line:trim() ~= "" then
        fishes = table.map(line:split(","), string.parseInt)
    end
end
local DAYS1 = 80
print("Part 1: ", table.fold(table.map(table.map(fishes, function(v) return DAYS1 - v end), lanternfish), 0, function(a, b) return a + b end))

local DAYS2 = 256
print("Part 2: ", table.fold(table.map(table.map(fishes, function(v) return DAYS2 - v end), lanternfish), 0, function(a, b) return a + b end))

-- // Part 1
--     const DAYS1: i64 = 80 + 1; // There's an obi-wan error somewhere
--     let res1: i64 = fishes.iter().map(|&v| DAYS1 - v).map(lanternfish).sum();
--     println!("{}", res1);

--     // Part 2
--     const DAYS2: i64 = 256 + 1; // There's an obi-wan error somewhere
--     let res2: i64 = fishes.iter().map(|&v| DAYS2 - v).map(lanternfish).sum();
--     println!("{}", res2);
