import sys
import re

keys = ("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

def is_valid1(ps):
    for key in keys:
        if (ps.find(" " + key + ":") == -1):
            return False
    return True

def validate_hgt(f):
    unit = f[-2:]
    val = f[:-2]
    if not re.match("\d*", val):
        return False
    val = int(val)
    # print(unit, val)
    return (unit == "cm" and 150 <= val <= 193) or (unit == "in" and 59 <= val <= 76)

validators = (
    lambda f: re.match("\d{4}", f) and 1920 <= int(f) <= 2002,
    lambda f: re.match("\d{4}", f) and 2010 <= int(f) <= 2020,
    lambda f: re.match("\d{4}", f) and 2020 <= int(f) <= 2030,
    validate_hgt,
    lambda f: re.match("#[0-9a-f]{6}", f),
    lambda f: f in ("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    lambda f: re.match("^\d{9}$", f),
)

def is_valid2(ps):
    for i, key in enumerate(keys):
        field_start = ps.find(" " + key + ":")
        if (field_start == -1):
            return False
        field_start += 5;
        field = ps[field_start:ps.find(" ", field_start)].strip()
        # print("Field", field)
        if not validators[i](field):
            return False
    return True

with open(sys.argv[1], "r") as infile:
    passport = ""
    good1 = 0
    good2 = 0
    while (line := infile.readline()):
        if (line == "\n"):
            if (is_valid1(passport)):
                good1 += 1
            if (is_valid2(passport)):
                good2 += 1
            passport = ""
        else:
            passport += " " + line + " "
    if (is_valid1(passport)):
        good1 += 1
    if (is_valid2(passport)):
        good2 += 1

print(good1)
print(good2)
