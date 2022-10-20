#include <stdio.h>  /* printf */
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "auxiliary.h"
#include "display.h"
#include "board.h"



/* Q2.2 : makes the player play the given color ;  temp argument is used only for simulation purposes, to test what would happen if you played a color */
static void play(char player, char temp, char color){
	int i = 0;
	int j = 0;
	int next = 1; // boolean-like value, 1 if one changed at least one tile, else 0
	while(next){
		next = 0;
		for(i = 0 ; i < BOARD_SIZE ; i++){
			for(j = 0 ; j < BOARD_SIZE ; j++){
				// If you're next to a tile of yours, and you're on a tile of the right color, you conquer it
				if(adj(player,temp,i,j) && get_cell(i,j) == color){
					set_cell(i,j,player);
					next = 1; // And we repeat the process
				}
			}
		}
	}
}




/* Q3.1 : The "real-player" strategy, used for human players */
void real_play(char player){
	play(player,TEMP,your_turn());
}

/* Q4.1 : The full-random strategy */
void random_play(char player){
	play(player,TEMP,'A'+(random()%(NB_COLORS)));
}

/* Q4.2 : The improved random strategy, where the AI only plays colors that allows it to gain tiles */
void improved_random_play(char player){
	int sc = score(player);
	char color='A';
	int stuck =0;
	int i,j;
	while(color < 'A' + NB_COLORS && stuck ==0){ // this part makes sure there exists a color that allows to gain tiles 
		play(TEMP,player,color);
			if(score(TEMP) > 0){
				stuck = 1;
			}
		for(i = 0 ; i < BOARD_SIZE ; i++){
			for(j = 0 ; j < BOARD_SIZE ; j++){
				if(get_cell(i,j) == TEMP){
					set_cell(i,j,color);
				}
			}
		}
		color++;
	}

	if(stuck ==1){
		while(score(player) == sc){// if the player isn't stuck he plays randomly until he gains territory
			random_play(player);
		}
	}

}

/* Q5.1 : The greedy strategy */
void greedy(char player){
	/* A basic maximum-search, where the value we have to maximise is the number of tiles we can gain, e.g. the potental score we can have. */
	int sc = score(player);
	char color = 'A';
	char max_color = 'A';
	for(color = 'A' ; color < 'A' + NB_COLORS ; color++){
		/* We emulate a move where TEMP is playing, and player has the role of the temporary color ; this way, it fills all tiles that the player would conquer (if he played colored) with TEMP */
		play(TEMP,player,color);
		if(score(player) + score(TEMP) > sc){
			sc = score(player)+score(TEMP);
			max_color = color;
		}
		else if(score(player) + score(TEMP) == sc){
				// 1/2 chances of keeping your current move, 1/2 chances of changing it, for the same surfacce
				if(rand()%2 == 1){
					max_color = color;	
				}
		}
		int i = 0;
		int j = 0;
		/* Getting rid of the emulation, by setting all TEMP-tiles to the color we supposed the player would play */
		for(i = 0 ; i < BOARD_SIZE ; i++){
			for(j = 0 ; j < BOARD_SIZE ; j++){
				if(get_cell(i,j) == TEMP){
					set_cell(i,j,color);
				}
			}
		}
	}
	/* Finally, plays with the best color */
	play(player,TEMP,max_color);	
}


// Q6.1 The spider player, who plays the color which gives him the largest border
void spider(char player){
	/* A basic maximum-search, where the value we have to maximise is the number of tiles we can gain, e.g. the potential border length */
	int max_bd = 0;
	int bd = 0;
	int sc = score(player);
	int sk = 0;
	char color = 'A';
	char max_color = 'A';

	for(color = 'A' ; color < 'A' + NB_COLORS ; color++){
		/* We emulate a move where TEMP is playing, and player has the role of the temporary color ; this way, it fills all tiles that the player would conquer (if he played colored) with TEMP */
		play(TEMP,player,color);

		// Computing the size of the new border, and the score you would get
		bd = border_size(player,TEMP);
		sk = score(player) + score(TEMP);

		// Take note that if all moves decrease the length of your border compared to the current length (without TEMP), you play the one who keep the coming border at maximum length
		// You count that move as a possible move only if it increases your score strictly
		if(sk > sc){
			// If you can strictly increase your border (compared to the other borders you calculated), just set your current move to that color
			if(bd > max_bd){
				max_bd = bd;
				max_color = color;
			}

			// If you get an equal border length to the one you got with another move, we randomise the change
			else if(bd == max_bd){
				// 1/2 chances of keeping your current move, 1/2 chances of changing it, for the same border length
				if(rand()%2 == 1){
					max_color = color;	
				}
			}
		}

		int i = 0;
		int j = 0;
		/* Getting rid of the emulation, by setting all TEMP-tiles to the color we supposed the player would play */
		for(i = 0 ; i < BOARD_SIZE ; i++){
			for(j = 0 ; j < BOARD_SIZE ; j++){
				if(get_cell(i,j) == TEMP){
					set_cell(i,j,color);
				}
			}
		}
	}

	/* Finally, plays with the best color */
	play(player,TEMP,max_color);

}





/* Q6.2 : The visionnary greedy strategy */
void double_greedy(char player){
	/* A basic maximum-search, where the value we have to maximise is the number of tiles we can gain, e.g. the potental score we can have. */
	int sc = score(player);
	char color = 'A';
	char color2 = 'A';
	char max_color = 'A';

	/* We Playing TEMP on color and TEMP2 on color2, to count what would get you the most tiles on the 2 next turns */
	for(color = 'A' ; color < 'A' + NB_COLORS ; color++){
		int i = 0;
		int j = 0;
		/* Emulating the moves */
		for(color2 = 'A' ; color2 < 'A' + NB_COLORS ; color2++){
			if(color2 == color){continue;}
			play(TEMP,player,color); // playing the first move with color
			play(TEMP2,player,color2); // Playing the second move, on two phases
			play(TEMP2,TEMP,color2);

			// Updating the score and the current move, if you got a better move
			if(score(player)+score(TEMP)+score(TEMP2) > sc){
				sc = score(player)+score(TEMP)+score(TEMP2);
				max_color = color;
			}
			else if(score(player)+score(TEMP)+score(TEMP2) == sc){
				// 1/2 chances of keeping your current move, 1/2 chances of changing it, for the same surfacce
				if(rand()%2 == 1){
					max_color = color;	
				}
			}

			// Getting rid of TEMP2 tiles
			for(i = 0 ; i < BOARD_SIZE ; i++){
				for(j = 0 ; j < BOARD_SIZE ; j++){
					if(get_cell(i,j) == TEMP2){
						set_cell(i,j,color2);
					}
				}
			}
		}

		// Getting rid of TEMP tiles
		for(i = 0 ; i < BOARD_SIZE ; i++){
			for(j = 0 ; j < BOARD_SIZE ; j++){
				if(get_cell(i,j) == TEMP){
					set_cell(i,j,color);
				}
			}
		}
	}

	/* Finally, plays with the best color */
	play(player,TEMP,max_color);	
}





void mix(char player){
	int max_bd=0;
	int bd=0;
	int min_bd=0; //il faudra lui donner la valeur de depart
	int sk=0;
	char color = 'A';
	char color2 = 'A';
	char max_color_g = 'A';
	char max_color_s = 'A';
	int sc = 0;
	int i = 0;
	int j = 0;

	for(color = 'A' ; color < 'A' + NB_COLORS ; color++){
		/* We emulate a move where TEMP is playing, and player has the role of the temporary color ; this way, it fills all tiles that the player would conquer (if he played colored) with TEMP */
		play(TEMP,player,color);
		// Computing the size of the new border, and the score you would get
		bd = border_size(player,TEMP);
		sk = score(TEMP);
		if(sk>0){
			// Emulating a double_greedy-type move
			/* Emulating the moves */
			for(color2 = 'A' ; color2 < 'A' + NB_COLORS ; color2++){
				if(color2 == color){continue;}
				play(TEMP,player,color); // playing the first move with color
				play(TEMP2,player,color2); // Playing the second move, on two phases
				play(TEMP2,TEMP,color2);
	
				// Updating the score and the current move, if you got a better move
				if(score(player)+score(TEMP)+score(TEMP2) > sc){
					sc = score(player)+score(TEMP)+score(TEMP2);
					max_color_g = color;
				}
				else if(score(player)+score(TEMP)+score(TEMP2) == sc){
					// 1/2 chances of keeping your current move, 1/2 chances of changing it, for the same surfacce
					if(rand()%2 == 1){
						max_color_g = color;	
					}
				}
	
				// Getting rid of TEMP2 tiles
				for(i = 0 ; i < BOARD_SIZE ; i++){
					for(j = 0 ; j < BOARD_SIZE ; j++){
						if(get_cell(i,j) == TEMP2){
							set_cell(i,j,color2);
						}
					}
				}
			}

/*
			if(sk>sc){
				sc = sk;
				max_color_g = color;
			}
			else if(sk == sc){// for identical prize 50% of changing move
				if(rand()%2 == 1){
					sc = score(TEMP);
					max_color_g = color;
				}
			}
*/

			// Emulating a spider-type move
			if(bd > max_bd){
				max_bd = bd;
				max_color_s = color;
			}
			else if(bd == max_bd){
				// 1/2 chances of keeping your current move, 1/2 chances of changing it, for the same border length
				if(rand()%2 == 1){
					max_bd = bd;
					max_color_s = color;	
				}
			}
		}


		/* Getting rid of the emulation, by setting all TEMP-tiles to the color we supposed the player would play */
		for(i = 0 ; i < BOARD_SIZE ; i++){
			for(j = 0 ; j < BOARD_SIZE ; j++){
				if(get_cell(i,j) == TEMP){
					set_cell(i,j,color);
				}
			}
		}
	}
	min_bd = max_bd-border_size(player,player);
	if(min_bd*3 < 2*sc){
		play(player,TEMP,max_color_g);
	}
	else{
		play(player,TEMP,max_color_s);
	}
}



/* Tests which strategy (among the strategies given above) the given-player will use ; this was implemented to simplify our tests ; the pointers on functions are here to give function as arguments */
static void strategy(char current_player, void (*strat1)(char), void (*strat2)(char)){
	if(current_player == PLAYER1){(*strat1)(current_player);}
	else{(*strat2)(current_player);}
}

/* Also here to simplify our tests ; runs the game according to the given strategies */
void run_game(char starting_player, void (*strat1)(char), void (*strat2)(char)){
   char current_player = starting_player;
   while(nextturn()){
	update_board();
	printf("Player 1 : %d\n",score(PLAYER1));
	printf("Player 2 : %d\n",score(PLAYER2));
	printf("\n");

	strategy(current_player,strat1,strat2); // The move is done here.

	// Switching player at the end of each turn.
	if(current_player == PLAYER1){
		current_player = PLAYER2;
	}
	else{
		current_player = PLAYER1;
	}
	//getchar();
   }
}

