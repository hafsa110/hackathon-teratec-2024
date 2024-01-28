## Build

Build all execs using: 
```python
source env.sh
python3 build.py
```

## Benchmarks

| Exec                     | Time (10 100000 100) in secs | Comments                                                    |
|--------------------------|-----------------------------|-------------------------------------------------------------|
| RiemannSiegel.acfl.orig  | 95.240                      | Original code, no comment                                    |
| RiemannSiegel.acfl.inline | 86.415                      | Inlining Z, C, Theta, even functions + compile-time math constants |
| RiemannSiegel.acfl.theta  | 20.930                      | Horner scheme + ARM intrinsics                                |
| RiemannSiegel.acfl.C      | 14.511                      | Horner scheme + ARM intrinsics + Memoization                   |
| RiemannSiegel.acfl.Z1     | 11.435                      | Consts + ARM intrinsics + Memoization + powers to square-roots |
| RiemannSiegel.acfl.Z2     | 4.945                       | FMOD optimization : https://stackoverflow.com/questions/26342823/implementation-of-fmod-function |
| RiemannSiegel.acfl.Z3     | 2.550                       | sin computation (transformation from cos) / Taylor / Horner  |
| RiemannSiegel.acfl.ZX     | 0.054                       | Parralel code w/ OpenMP + compilation w/ flag -mcpu=neoverse-v1 |
