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

local ALLLETTERS = {}

local function parseRule(line)
    line = line:trim()
    return {line:sub(0, 2), line:sub(7)}
end

local function step(st, rules)
    local newst = {}
    for _, rule in pairs(rules) do
        local cusu1 = rule[1]:sub(1, 1) .. rule[2]
        local cusu2 = rule[2] .. rule[1]:sub(2, 2)
        local occ = st[rule[1]] or 0
        newst[cusu1] = (newst[cusu1] or 0) + occ
        newst[cusu2] = (newst[cusu2] or 0) + occ
        if rule[1] == st.firstpair then
            newst.firstpair = cusu1
        end
        if rule[1] == st.lastpair then
            newst.lastpair = cusu2
        end
    end
    return newst
end

local function occurrences(st, letter)
    local occs = 0
    for k, v in pairs(st) do
        if letter == k:sub(1, 1) then
            -- if v > 0 then
            --     print("Letter", letter, "found in pair", k)
            --     print("Adding occurrences", v)
            -- end
            occs = occs + v
        end
        if letter == k:sub(2, 2) then
            -- if v > 0 then
            --     print("Letter", letter, "found in pair", k)
            --     print("Adding occurrences", v)
            -- end
            occs = occs + v
        end
    end
    if letter == st.firstpair:sub(1, 1) then
        -- print("Letter", letter, "in first pair", st.firstpair)
        occs = occs + 1
    end
    if letter == st.lastpair:sub(2, 2) then
        -- print("Letter", letter, "in last pair", st.lastpair)
        occs = occs + 1
    end
    -- if occs % 2 ~= 0 then
    --     print("============================ AAAAAH", letter)
    -- end
    return occs / 2
end

local function length(st)
    local l = 0
    for _, letter in pairs(ALLLETTERS) do
        l = l + occurrences(st, letter)
    end
    return l
end

local function findmaxmin(st)
    local occs = table.map(ALLLETTERS, function(l) return occurrences(st, l) end)
    local max = table.fold(occs, 0, function(acc, val) return math.max(acc, val) end)
    local min = table.fold(occs, max, function(acc, val) return math.min(acc, val) end)
    return max, min, max - min
end

local templ;
local rules = {};
local i = 0
for line in io.lines(filename) do
    if i == 0 then
        templ = line:trim()
    elseif i > 1 then
        local newrule = parseRule(line)
        table.insert(rules, newrule)
        table.insert(ALLLETTERS, newrule[1]:sub(1, 1))
        table.insert(ALLLETTERS, newrule[1]:sub(2, 2))
        table.insert(ALLLETTERS, newrule[2])
    end
    i = i + 1
end
ALLLETTERS = table.unique(ALLLETTERS)

local initst = {}
for i=1,(templ:len() - 1) do
    local cusu = templ:sub(i, i + 1)
    -- print(cusu)
    initst[cusu] = initst[cusu] and initst[cusu] + 1 or 1
end
initst.firstpair = templ:sub(1, 2)
initst.lastpair = templ:sub(templ:len() - 1)

-- for pair, occ in pairs(step(initst, rules)) do
-- -- for pair, occ in pairs(initst) do
--     print(pair, occ)
-- end
-- print(length(initst))
-- print(length(step(initst, rules)))
-- print(length(step(step(initst, rules), rules)))

local st = initst
for _=1,10 do
    st = step(st, rules)
    -- print(length(st))
end
print(findmaxmin(st))

for _=11,40 do
    st = step(st, rules)
end
print(findmaxmin(st))
