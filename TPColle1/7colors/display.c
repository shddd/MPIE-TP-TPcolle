#include <stdio.h>  /* printf */
#include <stdlib.h>
#include <SDL/SDL.h>
#include "board.h"


#define TILE_SIDE 15


/* All necessary functions to handle the graphic interface */




static SDL_Surface *screen = NULL; // Global variable which represents the screen


// Initialises the window
void init_window(){
	screen = SDL_SetVideoMode(TILE_SIDE*BOARD_SIZE, TILE_SIDE*BOARD_SIZE, 32, SDL_HWSURFACE);
}


// Returns the color (in the RGB system) associated to each color in the game
static Uint32 color(char letter){
	switch(letter){
		case 'A': return SDL_MapRGB(screen->format, 102, 0, 0);// brown
		case 'B': return SDL_MapRGB(screen->format, 255, 0, 0); // red
		case 'C': return SDL_MapRGB(screen->format, 255, 128, 0); // orange
		case 'D': return SDL_MapRGB(screen->format, 255, 255, 0); // yellow
		case 'E': return SDL_MapRGB(screen->format, 0, 255, 0); // green
		case 'F': return SDL_MapRGB(screen->format, 0, 0, 255); // blue
		case 'G': return SDL_MapRGB(screen->format, 127, 0, 255); // purple
		case PLAYER1: return SDL_MapRGB(screen->format, 0, 0, 0); // black
		case PLAYER2: return SDL_MapRGB(screen->format, 255, 255, 255); // white
		case TEMP: return SDL_MapRGB(screen->format, 127, 127, 127); // gray ; this one was used only for debugging purposes
		case TEMP2: return SDL_MapRGB(screen->format, 64, 64, 64); // light gray, for debugging purposes
		default: return SDL_MapRGB(screen->format, 0, 255, 255); //cyan ; just in case there's a problem, we can detect it
	}
}


// Updates the contents of the windows, by drawing each tile again
void update_board(){
	int i;
	int j;
	SDL_Surface *tile = NULL;
	SDL_Rect position;

	for(i = 0 ; i < BOARD_SIZE ; i++){
		for(j = 0 ; j < BOARD_SIZE ; j++){
			tile = SDL_CreateRGBSurface(SDL_HWSURFACE, TILE_SIDE, TILE_SIDE, 32, 0, 0, 0, 0);
			SDL_FillRect(tile, NULL, color(get_cell(i,j)));
			position.y = i*TILE_SIDE;
			position.x = j*TILE_SIDE;
			SDL_BlitSurface(tile, NULL, screen, &position);
		}
	}
	SDL_Flip(screen); /* Updating what we see */
	SDL_FreeSurface(tile);
}
