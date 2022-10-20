#include <stdio.h>  /* printf */
#include <stdlib.h>
#include <math.h>
#include <time.h>

/* We want a 30x30 board game by default */
#define BOARD_SIZE 30 
#define NB_COLORS 7
#define PLAYER1 '*'
#define PLAYER2 '-'
#define TEMP '+' // This is q temporary color, used for simulation purposes
#define TEMP2 '=' // Also a temporary color for simulation purposes, for double greedy

char get_cell(int x, int y) ;

void set_cell(int x, int y, char color);

void copy_board();

void get_saved_board();

void print_board();

void random_filling();
