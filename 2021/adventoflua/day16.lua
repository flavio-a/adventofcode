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
local hex2bin = {
    ["0"] = "0000",
    ["1"] = "0001",
    ["2"] = "0010",
    ["3"] = "0011",
    ["4"] = "0100",
    ["5"] = "0101",
    ["6"] = "0110",
    ["7"] = "0111",
    ["8"] = "1000",
    ["9"] = "1001",
    ["a"] = "1010",
    ["b"] = "1011",
    ["c"] = "1100",
    ["d"] = "1101",
    ["e"] = "1110",
    ["f"] = "1111",
}

local function parse_literal_payload(str)
    local i = 1 - 5
    local res = ""
    repeat
        i = i + 5
        res = res .. str:sub(i + 1, i + 4)
    until str:sub(i, i) == "0"
    return tonumber(res, 2), str:sub(i + 5)
end

local function parse_packet(str)
    local res = {}
    local i = 1
    res.version = tonumber(str:sub(1, 3), 2)
    res.typeid = tonumber(str:sub(4, 6), 2)
    if res.typeid == 4 then
        local cusu, rest = parse_literal_payload(str:sub(7))
        res.literal = cusu
        return res, rest
    else -- Operator
        local lentypeid = str:sub(7, 7)
        if lentypeid == "0" then
            local totlen = tonumber(str:sub(8, 8 + 15 - 1), 2)
            local subpackets = str:sub(8 + 15, 8 + 15 + totlen - 1)
            res.subpackets = {}
            while subpackets ~= "" do
                local cusu, rest = parse_packet(subpackets)
                table.insert(res.subpackets, cusu)
                subpackets = rest
            end
            return res, str:sub(8 + 15 + totlen)
        else
            local numsubpacks = tonumber(str:sub(8, 8 + 11 - 1), 2)
            local subpackets = str:sub(8 + 11)
            res.subpackets = {}
            for i=numsubpacks,1,-1 do
                local cusu, rest = parse_packet(subpackets)
                table.insert(res.subpackets, cusu)
                subpackets = rest
            end
            assert(#res.subpackets == numsubpacks, "Problems matching length of operator type 1")
            return res, subpackets
        end
    end
end

local function sum_version_nums(packet)
    local sum = packet.version
    if packet.subpackets then
        sum = table.fold(packet.subpackets, sum, function (a, b) return a + sum_version_nums(b) end)
    end
    return sum
end

local function eval_packet(packet)
    if packet.typeid == 4 then
        return packet.literal
    elseif packet.typeid == 0 then
        return table.fold(packet.subpackets, 0, function(acc, p) return acc + eval_packet(p) end)
    elseif packet.typeid == 1 then
        return table.fold(packet.subpackets, 1, function(acc, p) return acc * eval_packet(p) end)
    elseif packet.typeid == 2 then
        return table.fold(packet.subpackets, eval_packet(packet.subpackets[1]), function(acc, p) return math.min(acc, eval_packet(p)) end)
    elseif packet.typeid == 3 then
        return table.fold(packet.subpackets, eval_packet(packet.subpackets[1]), function(acc, p) return math.max(acc, eval_packet(p)) end)
    elseif packet.typeid == 5 then
        local sp1 = eval_packet(packet.subpackets[1])
        local sp2 = eval_packet(packet.subpackets[2])
        return sp1 > sp2 and 1 or 0
    elseif packet.typeid == 6 then
        local sp1 = eval_packet(packet.subpackets[1])
        local sp2 = eval_packet(packet.subpackets[2])
        return sp1 < sp2 and 1 or 0
    elseif packet.typeid == 7 then
        local sp1 = eval_packet(packet.subpackets[1])
        local sp2 = eval_packet(packet.subpackets[2])
        return sp1 == sp2 and 1 or 0
    end
end

local inputstr
for line in io.lines(filename) do
    inputstr = line:trim():gsub("([%a%d])", function(letter) return hex2bin[letter:lower()] end)
end
local packet = parse_packet(inputstr)
-- print(require"dumper"(packet))

print("Part 1: ", sum_version_nums(packet))

print("Part 2: ", eval_packet(packet))
