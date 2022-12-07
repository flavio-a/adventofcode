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
local dumper = require('dumper')

local function get_by_name(children, name)
    return children[table.find(children, function(child)
        return child.type == "D" and child.name == name
    end)]
end

local function compute_tot_size(root)
    if root.type == "F" then
        return root.size
    end
    if not root.tot_size then
        root.tot_size = table.fold(root.children, 0, function(acc, child)
            return acc + compute_tot_size(child)
        end)
    end
    return root.tot_size
end

local function filter_and_sum(root)
    if root.type == "F" then
        return 0
    end
    return table.fold(root.children, root.tot_size <= 100000 and root.tot_size or 0, function(acc, child)
        return acc + filter_and_sum(child)
    end)
end

local function part2(root, min_size)
    if root.type == "F" then
        return 70000000
    end
    if root.tot_size < min_size then
        return 70000000
    end
    return table.fold(root.children, root.tot_size, function(acc, child)
        return math.min(acc, part2(child, min_size))
    end)
end

local filename = str.trim(arg[1])

local fs = { type = "D", name = "/", children = {}, parent = fs }
local curr = fs
for line in io.lines(filename) do
    line = line:trim()
    if line:sub(1, 1) == "$" then
        local cmd = line:sub(3, 4)
        if cmd == "cd" then
            local target = line:sub(6)
            if target == "/" then
                curr = fs
            elseif target == ".." then
                curr = curr.parent
            else
                curr = get_by_name(curr.children, line:sub(6))
            end
            if not curr then
                print("AAAAA")
            end
        elseif cmd == "ls" then
            -- Nothing, expects next lines not to be commands
        else
            error("Unexpected plot twist")
        end
    else
        local newchild = {}
        if line:sub(1,3) == "dir" then
            newchild.type = "D"
            newchild.name = line:sub(5)
            newchild.children = {}
            newchild.parent = curr
        else
            newchild.type = "F"
            newchild.size, newchild.name = line:match("^(%d+) (.+)$")
            newchild.size = tonumber(newchild.size)
        end
        table.insert(curr.children, newchild)
    end
end

print("Day 7")

compute_tot_size(fs)
print(filter_and_sum(fs))

local missing_space = 30000000 - (70000000 - fs.tot_size)
print(part2(fs, missing_space))
