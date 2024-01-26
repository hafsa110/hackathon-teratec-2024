## Build

Build all execs using: 
```python
python3 build.py
```

## Benchmarks

| Exec                     | Time (10^5 100 100) in secs | Comments                                                    |
|--------------------------|-----------------------------|-------------------------------------------------------------|
| RiemannSiegel.acfl.orig  | 95.240                      | Original code, no comment                                    |
| RiemannSiegel.acfl.inline | 86.415                      | Inlining Z, C, Theta, even functions + compile-time math constants |
| RiemannSiegel.acfl.theta  | 20.930                      | Horner scheme + ARM intrinsics                                |
| RiemannSiegel.acfl.C      | 14.511                      | Horner scheme + ARM intrinsics + Memoization                   |
| RiemannSiegel.acfl.Z1     | 11.435                      | Consts + ARM intrinsics + Memoization + Conversion des puissances en racine carrée |
| RiemannSiegel.acfl.Z2     | 4.945                       | Optimization des fmod (cf. Mehdi) : https://stackoverflow.com/questions/26342823/implementation-of-fmod-function |
| RiemannSiegel.acfl.Z3     | 2.550                       | Intrinsic du sin (conversion depuis cos) / Taylor / Horner  |
| RiemannSiegel.acfl.ZX     | 0.054                       | Parallélisation avec OpenMP + Compile avec flag -mcpu=neoverse-v1 |
