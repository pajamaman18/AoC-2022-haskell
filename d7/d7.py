import re

f = open("input.txt")

list = []
for line in f:
    l = line.rstrip('\n').lstrip("$ ")
    if l != "ls":
        list += [l]


def splitList(list):
    for i in range(0, len(list)):
        # print (list[:i])
        if list[i][:2] == "cd":
            return [list[:i]] + [splitSubDir(list[i:])]
    return []


def bottom(l):
    if l[-1][:2] == 'cd ..' and l[-2][:2] != 'cd ..':
        return True
    else:
        False


def splitSubDir(list):
    depth = 0
    # print(list)
    if not (bottom(list)):
        for i in range(0, len(list)):
            if list[i][:2] == "cd":
                # print(list[i])
                if list[i][2:] == " ..":
                    depth -= 1
                else:
                    depth += 1
            # print(depth)
            if depth == 0:
                return [blocksplit(list[:i+1])] + [splitSubDir(list[i+1:])]
    return list


def blocksplit(list):
    for i in range(1, len(list)):
        if list[i][:2] == "cd":
            return [list[:i]] + [splitSubDir(list[i:])]


def getsize(str):
    if str[0].isdigit():
        return int(re.search(r'\d+', str).group())
    else:
        return 0


def folderSize(tree):
    out = []
    for elem in tree:
        # print(type(elem[0]) )
        if type(elem[0]) == str:
            out += [getlsize(elem)]
        else:
            for subdir in elem:
                out += [folderSize(subdir)]
    # print(tree)
    return out

# def out(list):
#     size = 0
#     for elem in list:
#         if type(elem) == int:
#             size += elem
#         else:

def cleanup(list):
    out = []
    for elm in list:
        if type(elm) == int:
            out += [elm]
        elif all(elm):
            out += [sum(elm)]

        else:
            out += [cleanup(elm)]


def getlsize(list):
    size = 0
    for elem in list:
        size += getsize(elem)
    return size

# print(list)
treelist = splitList(list[1:])
# print(treelist)
sizelist = folderSize(treelist)
print(cleanup(sizelist))