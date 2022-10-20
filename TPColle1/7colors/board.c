#include <stdio.h>  /* printf */
#include <stdlib.h>
#include <math.h>
#include <time.h>

/* We want a 30x30 board game by default */
#define BOARD_SIZE 30 
#define NB_COLORS 7
#define PLAYER1 '*'
#define PLAYER2 '-'

/** Represent the actual current board game 
 * 
 * NOTE: global variables are usually discouraged (plus encapsulation in
 *  an appropriate data structure would also be preferred), but don't worry. 
 *  For this first assignment, no dinosaure will get you if you do that. 
 */
char board[BOARD_SIZE * BOARD_SIZE] = { 65 }; // Filled with zeros
char saved_board[BOARD_SIZE * BOARD_SIZE] = { 65 }; // Another global variable to store a board, just in case

/** Retrieves the color of a given board cell */
char get_cell(int x, int y) 
{
   return board[y*BOARD_SIZE + x];
}

/** Changes the color of a given board cell */
void set_cell(int x, int y, char color) 
{
   board[y*BOARD_SIZE + x] = color;
}

/* Copy the board into the save slot */
void copy_board(){
	int i = 0;
	int j = 0;
	for(i = 0 ; i < BOARD_SIZE ; i++){
		for(j = 0 ; j < BOARD_SIZE ; j++){
			saved_board[j*BOARD_SIZE + i] = get_cell(i,j);
		}
	}
}

/* Gets the board from the save slot and copy it into the actual board slot */
void get_saved_board(){
	int i = 0;
	int j = 0;
	for(i = 0 ; i < BOARD_SIZE ; i++){
		for(j = 0 ; j < BOARD_SIZE ; j++){
			set_cell(i,j,saved_board[j*BOARD_SIZE + i]);
		}
	}
}

/** Prints the current state of the board on screen
 * 
 * Implementation note: It would be nicer to do this with ncurse or even 
 * SDL/allegro, but this is not really the purpose of this assignment.
 */
void print_board() 
{
   int i, j;
   for (i=0; i<BOARD_SIZE; i++) {
      for (j=0; j<BOARD_SIZE; j++) 
	 printf("%c", get_cell(i, j));
      printf("\n");
   }
}

/* Q2.1 : Randomly fills each tile with a color */
void random_filling(){
	int i = 0;
	int j = 0;
	for(i = 0 ; i < BOARD_SIZE ; i++){
		for(j = 0 ; j < BOARD_SIZE ; j++){
			set_cell(i,j,'A'+(random()%(NB_COLORS)));
		}
	}
	set_cell(0,BOARD_SIZE-1,PLAYER1);
	set_cell(BOARD_SIZE-1,0,PLAYER2);
}
