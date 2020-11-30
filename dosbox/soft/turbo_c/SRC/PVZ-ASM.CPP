#include <stdio.h>
int main(){
     unsigned int a;

     asm{
	mov a, 0x123;
	mov ax, a;
	add ax, 15;
	mov a, ax

     }

     printf("%d \n",a);
     return 0;

}