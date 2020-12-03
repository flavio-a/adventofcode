nums = []
with open("input1", "r") as f:
    for l in f:
        nums.append(int(l))

def find_sum(nums, sum):
    """Given a sorted list and a sum, finds the pair of elements with that sum
    """
    i = 0
    j = len(nums) - 1
    while (i < j):
        if nums[i] + nums[j] == sum:
            return (nums[i], nums[j])
        elif nums[i] + nums[j] < sum:
            i += 1
        else: # nums[i] + nums[j] > sum:
            j -= 1
    return None

nums.sort()

n, m = find_sum(nums, 2020)
print(n * m)

# For each first number, it performs the algorithm for two elements. Probably
# it can be improved reusing again informations about indexes found for a fixed
# l, but it's fast enough like this
for (i, l) in enumerate(nums):
    res = find_sum(nums[i + 1:], 2020 - l)
    if res:
        print(l * res[0] * res[1])
        break
