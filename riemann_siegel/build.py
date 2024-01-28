#!/usr/bin/python3
import os

revs = ["orig", "inline", "theta", "C", "Z1", "Z2", "Z3", "ZX"]
cxxs = {
    "acfl": "armclang++ -O3 -mcpu=neoverse-V1 -fopenmp",
    "gnu": "g++ -O3 -march=native -fopenmp",
}

def run(cmd):
    print(f"@ {cmd}")
    os.system(cmd)

run("mkdir -p ./bin/")
for (tag, cxx) in cxxs.items():
    for rev in revs:
        cmd = f"{cxx} RiemannSiegel-{rev}.cpp -o ./bin/RiemannSiegel.{tag}.{rev}"
        run(cmd)
print(f"Done building {len(cxxs) * len(revs)} execs.")