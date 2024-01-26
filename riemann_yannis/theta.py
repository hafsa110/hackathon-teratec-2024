import math

def theta(t):
    pi = 3.1415926535897932385
    return (t / 2.0 * math.log(t / (2.0 * pi)) - t / 2.0 - pi / 8.0 +
            1.0 / (48.0 * t) + 7.0 / 5760.0 / (t ** 3.0) +
            31.0 / 80640.0 / (t ** 5.0) + 127.0 / 430080.0 / (t ** 7.0) +
            511.0 / 1216512.0 / (t ** 9.0))

# Exemple d'utilisation
t = 10000.0
for _ in range(5):
    tt = theta(t)
    print(f"t = {t:12} ; theta(t) = {tt} ; sqrt(tt) = {math.sqrt( theta(t/10) )}")
    t = 10.0 * t
