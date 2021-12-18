from collections import deque

def parseInput(contents):
    edges = [l.split("-") for l in contents.strip().split("\n")]
    edges_set = set([])
    for (f, t) in edges:
        edges_set.add((f, t))
        if t != "end" and f != "start":
            edges_set.add((t, f))

    return edges_set

def visitableNeighbours(edges, loc):
    fr = loc[0]
    banned = [e for e in loc if e.islower()]
    for (f, t) in edges:
        if f == fr and t not in banned:
            yield t


edges = parseInput(open("./input").read())

locs = deque() # each element is a list of all the visited nodes
finished = set()
locs.append(["start"])
while len(locs) > 0:
    l = locs.pop()
    if l[0] == "end":
        finished.add("-".join(l))
    else:
        for neighbour in visitableNeighbours(edges, l):
            locs.append([neighbour] + l)

print(len(finished))
