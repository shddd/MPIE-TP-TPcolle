#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "auxiliary.h"

void real_play(char player);

void random_play(char player);

void improved_random_play(char player);

void greedy(char player);

void spider(char player);

void double_greedy(char player);

void mix(char player);

void run_game(char starting_player, void (*strat1)(char), void (*strat2)(char));
