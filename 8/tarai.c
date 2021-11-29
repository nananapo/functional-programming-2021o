#include <stdio.h>

int tarai(int x,int y,int z){
	if(x <= y) return y;
	else return tarai(tarai(x-1,y,z),tarai(y-1,z,x),tarai(z-1,x,y));
}

main() {
	printf("%d\n",tarai(20,10,5));
}
