all: 7colors

7colors: 7colors.o
	gcc *.o -o 7colors `sdl-config --cflags --libs` 

7colors.o: 7colors.c board.c auxiliary.c strategies.c display.c
	gcc -Wall -Werror -Wextra -lSDL_image *.c -c -g
clean:
	rm -rf *.o

mrproper: clean
	rm -rf 7colors
