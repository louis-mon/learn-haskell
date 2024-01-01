from sympy import *

with open("input.txt", 'r') as file:
    lines = file.readlines()

# Parse p and v from lines
data = [line.strip().split(' @ ') for line in lines]
p = [list(map(float, point.split(','))) for point,_ in data]
v = [list(map(float, velocity.split(','))) for _, velocity in data]
t = symbols(f"t:{len(p)}")
pf = symbols("pf:3")
vf = symbols("vf:3")
eqs = []
for i in range(len(t)):
    for j in range(3):
        eqs.append(p[i][j] - pf[j] + t[i] * (v[i][j] - vf[j]))

res = solve(eqs, (*t,*pf,*vf), dict=True,rational=True)

print(len(res))
pfx = res[0][pf[0]]
pfy = res[0][pf[1]]
pfz = res[0][pf[2]]
print(res)
print(pfx,pfy,pfz)
print(pfx+pfy+pfz)