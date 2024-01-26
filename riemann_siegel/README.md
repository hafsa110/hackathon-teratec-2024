## Build

Build all execs using: 
```python
python3 build.py
```

## Benchmarks

    Exec                      |   Time (10 100000 100) en secs    |   Comments
=========================================================================================
RiemannSiegel.acfl.orig       |                 95.240            | Original code no comment
RiemannSiegel.acfl.inline     |                 86.415            | Inlining Z, C, Theta, even functions + compile time math constants
RiemannSiegel.acfl.theta      |                 20.930            | Horner scheme + ARM intrisics
RiemannSiegel.acfl.C          |                 14.511            | Horner scheme + ARM intrisics + Memoization
RiemannSiegel.acfl.Z1         |                 11.435            | Consts + ARM intrisics + Memoization + Conversion des puissances en racine carrée
RiemannSiegel.acfl.Z2         |                 4.945             | Optimization des fmod (cf. Mehdi) : https://stackoverflow.com/questions/26342823/implementation-of-fmod-function
RiemannSiegel.acfl.Z3         |                 2.550             | Intrisinc du sin (conversion depuis cos) / Taylor / Horner
RiemannSiegel.acfl.ZX         |                 0.054             | Parallélisation avec OpenMP + Compile avec flag -mcpu=neoverse-v1