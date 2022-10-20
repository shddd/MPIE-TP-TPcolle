#include <stdio.h>  /* printf */
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "board.h"

int adj(char player, char temp, int i, int j);

char your_turn();

int score(char player);

int nextturn();

int border_size(char player, char temp);
