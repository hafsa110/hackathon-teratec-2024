import os

revs = ["orig", "inline", "theta", "C", "Z1", "Z2", "Z3", "ZX"]
cxxs = {
    "acfl": "armclang++ -O3 -mcpu=neoverse-V1 -fopenmp",
    "gnu": "g++ -O3 -march=native -fopenmp",
}

os.makedirs('./bin/', exist_ok=True)
for (tag, cxx) in cxxs.items():
    for rev in revs:
        cmd = f"{cxx} RiemannSiegel-{rev}.cpp -o ./bin/RiemannSiegel.{tag}.{rev}"
        print(f"@ {cmd}")
        os.system(cmd)