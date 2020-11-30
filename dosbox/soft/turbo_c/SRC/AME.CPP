#include <stdio.h>

int main(){
   unsigned int w;

   asm {
     mov ax, 3
     add ax, 5
     mov w, ax
   }

   printf("%d\n",w);


}