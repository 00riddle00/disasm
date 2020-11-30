#include <stdio.h>

int g(int g1, float *g2) {
    return 5;
}

int f(int i, long l) {
    float x,y;
    i = g(l, &x);

    return i;
}

int main() {
    static int a; long b;
    a = f(a,b);

    return 0;
}

