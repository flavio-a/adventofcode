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

local function parse_num(line)
    -- Four cases, since Lua doesn't have the regex | operator
    -- Two subtrees =================================
    local s1, s2 = line:match("^%[(%b[]),(%b[])%]$")
    if s1 then
        return { parse_num(s1), parse_num(s2) }
    end
    -- ==============================================
    local s1, c2 = line:match("^%[(%b[]),(%d+)%]$")
    if s1 then
        return { parse_num(s1), tonumber(c2) }
    end
    -- ==============================================
    local c1, s2 = line:match("^%[(%d+),(%b[])%]$")
    if c1 then
        return { tonumber(c1), parse_num(s2) }
    end
    -- ==============================================
    local c1, c2 = line:match("^%[(%d+),(%d+)%]$")
    if c1 then
        return { tonumber(c1), tonumber(c2) }
    end
    assert(false, "Error while parsing")
end

-- Adds val to the leftmost/rightmost number of pair. Leftmost or rightmost
-- depends on lr, either being 1 (left) or 2 (right)
local function addside(pair, val, lr)
    if type(pair) == "number" then
        return pair + val
    end
    pair[lr] = addside(pair[lr], val, lr)
    return pair
end

local function explode(pair)
    -- The inner function looks for the first guy to explode and explodes it,
    -- reporting up what should be added to the left and to the right
    -- Returns (in order) add_left, the new snailfish_number, add_right
    local function _explode(pair, depth)
        if type(pair) ~= "table" then
            return 0, pair, 0
        end
        if depth >= 4 then -- External call with depth == 0
            -- Explode pair
            -- print("Exploding pair", pair[1], pair[2])
            return pair[1], 0, pair[2]
        end
        -- Explodes left
        local addl, new_subpl, laddr = _explode(pair[1], depth + 1)
        if laddr > 0 then
            pair[2] = addside(pair[2], laddr, 1)
        end
        pair[1] = new_subpl
        -- Explodes right
        local raddl, new_subpr, addr = _explode(pair[2], depth + 1)
        if raddl > 0 then
            pair[1] = addside(pair[1], raddl, 2)
        end
        pair[2] = new_subpr
        return addl, pair, addr
    end

    local _, res, _ = _explode(pair, 0)
    return res
end

local function split(pair)
    if type(pair) ~= "table" then
        if pair > 9 then
            return { math.floor(pair / 2), math.floor(pair / 2 + 0.5) }, true
        else
            return pair, false
        end
    end
    local done = false
    pair[1], done = split(pair[1])
    if not done then
        pair[2], done = split(pair[2])
    end
    return pair, done
end

-- I should check idempotency of explode...
local function reduce(pair)
    local res, cont = split(explode(pair))
    if cont then
        return reduce(res)
    else
        return res
    end
end

local function add(pair1, pair2)
    return reduce{pair1, pair2}
end

local function fold1(tab, func)
    local zero = table.remove(tab, 1)
    return table.fold(tab, zero, func, ipairs)
end

local function magnitude(pair)
    if type(pair) ~= "table" then
        return pair
    end
    return 3 * magnitude(pair[1]) + 2 * magnitude(pair[2])
end

local nums = {}
for line in io.lines(filename) do
    table.insert(nums, parse_num(line:trim()))
end
-- print(require"dumper"(nums[1]))
-- print"====================================="
local cusu = fold1(table.copy(nums), add)
-- print(require"dumper"(cusu))
print(magnitude(cusu))

-- Part 2
local maxmagn = 0
for _, p1 in ipairs(nums) do
    for _, p2 in ipairs(nums) do
        -- Can I add a number with itself? I assume so
        -- The table.copy are needed because reduce works in-place
        local magn = magnitude(add(table.copy(p1), table.copy(p2)))
        if magn > maxmagn then
            maxmagn = magn
        end
    end
end
print(maxmagn)