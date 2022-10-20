#include <stdio.h>  /* printf */
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "board.h"
#include "display.h"

/* Returns 1 if there is a case nearby which belongs to the given player or which has the temporary color (for simulation purposes), else 0*/
int adj(char player, char temp, int i, int j){
	int up =0;
		if((get_cell(i-1,j) == player || get_cell(i-1,j) == temp) && i !=0){up =1;}
	int left =0;
		if((get_cell(i,j-1) == player || get_cell(i,j-1) == temp) && j !=0){left =1;}
	int down =0;
		if((get_cell(i+1,j) == player || get_cell(i+1,j) == temp) && i != BOARD_SIZE-1){down =1;}
	int right =0;
		if((get_cell(i,j+1) == player || get_cell(i,j+1) == temp) && j != BOARD_SIZE-1){ right =1;}
	if(up || down || right || left){
		return 1;
	}
	else{
		return 0;
	}
}


// Returns 1 if the tile you can reach from (i,j) walking in the given direction isn't conquered by the player or under temporary influence
// Else, returns 0 ; for the directions : 0 is North, 1 is East, 2 is South, 3 is West
static int adj_free(char player, char temp, int i, int j, int direction){
	if(direction == 0 && get_cell(i-1,j) != player && get_cell(i-1,j) != temp && i != 0){return 1;}
	else if(direction == 1 && get_cell(i,j+1) != player && get_cell(i,j+1) != temp && j != BOARD_SIZE-1){return 1;}
	else if(direction == 2 && get_cell(i+1,j) != player && get_cell(i+1,j) != temp && i != BOARD_SIZE-1){return 1;}
	else if(direction == 3 && get_cell(i,j-1) != player && get_cell(i,j-1) != temp && j != 0){return 1;}
	else {return 0;}
}

// Returns the index of the row you arrive on if you walk in the given direction from row i
static int neighbour_row(int i, int direction){
	if(direction == 0){return i-1;}
	else if(direction == 2){return i+1;}
	else {return i;}
}

// Returns the index of the column you arrive on if you walk in the given direction from column j
static int neighbour_column(int j, int direction){
	if(direction == 1){return j+1;}
	else if(direction == 3){return j-1;}
	else {return j;}
}


// Returns 1 if the given tile is in the game board, else returns 0
static int valid_tile(int i, int j){
	int test = 1;
	//if((i >= BOARD_SIZE) || (j >= BOARD_SIZE) || (i < 0) || (j < 0)){test = 0;}
	if(i >= BOARD_SIZE || j >= BOARD_SIZE){test = 0;}
	else if(j >= BOARD_SIZE){test = 0;}
	else if (i < 0){test = 0;}
	else if (j < 0){test = 0;}
	return test;
}

/* Q3.1 : Returns the color given by a human player on the keyboard*/
char your_turn(){
	char color = 0;
	do{
		printf("Choose a color between A and G : \n");
		printf("A : Brown\nB : Red\nC : Orange\nD : Yellow\nE : Green\nF : Blue\nG : Purple\n");
		color = getchar();
	}while(color > 'G' || color < 'A');
	return color;
}

/* Returns the number of tiles belonging to the given player */
int score(char player){
	int i = 0;
	int j = 0;
	int r = 0;
	for(i = 0 ; i < BOARD_SIZE ; i++){
		for(j = 0 ; j < BOARD_SIZE ; j++){
			if(get_cell(i,j) == player){r+=1;}
		}
	}
	return r;
}

/* Q3.2 : Returns a boolean-like value, 0 if the game has to stop (e.g. if one player conquered more than half of the grid), 1 if the show must go on */
int nextturn(){
	int p1 = score(PLAYER1);
	int p2 = score(PLAYER2);
	if(p1 >= (BOARD_SIZE*BOARD_SIZE)/2 || p2 >= (BOARD_SIZE*BOARD_SIZE)/2){
		return 0;
	}
	else{
		return 1;
	}
}




// Calculating the size of the border that the player would get, considering that several tiles are now TEMP because he played a color
// We start from the initial position corresponding to the player, and walk on the border of his territory, counting accessible cases in the outside
int border_size(char player, char temp){
	int size = 0; // size of the border

	// Position of the starting point
	int start_i = 0;
	int start_j = 0;

	// Actual position
	int i = 0;
	int j = 0;

	// After you started, you will go back to the start at most twice ; thus this is used as a condition to stop the march
	int start_visited = 0;
	int direction = 0; // 0 : North ; 1 : East ; 2 : South ; 3 : West
	int complete_turn = 0; // in case you always turn left and stay in the same position, this allows you to stop

	// Initializing the starting point
	if(player == PLAYER1 || temp == PLAYER1){
		start_j = BOARD_SIZE-1;
		direction = 3;
	}
	else if(player == PLAYER2 || temp == PLAYER2){
		start_i = BOARD_SIZE-1;
		direction = 1;
	}
	i = start_i;
	j = start_j;



	// Like in a labyrinth, you try to always turn in the same direction while you can
	while(start_visited < 2 && complete_turn < 4){

		// If there is NOTHING (but the edge of the board) on the right
		if(!valid_tile(neighbour_row(i,(direction+1)%4),neighbour_column(j,(direction+1)%4))){
			// If theres a tile in front of you
			if(valid_tile(neighbour_row(i,direction),neighbour_column(j,direction))){
				// And if you conquered it, then just go forward
				if(!adj_free(player,temp,i,j,direction)){
					complete_turn = 0;
					i = neighbour_row(i,direction);
					j = neighbour_column(j,direction);
				}
				// Else just turn left
				else{
					direction = (direction-1)%4;
					if(direction < 0){direction += 4;} // This is done because the % operator sometimes gives negative int
					complete_turn += 1;
				}
			}
			// If there's the edge of the board in front of you, just turn left
			else{
				direction = (direction-1)%4;
				if(direction < 0){direction += 4;}
				complete_turn += 1;
			}
		}

		// If there's a tile on your right
		else{
			char neighbour_color = get_cell(neighbour_row(i,(direction+1)%4),neighbour_column(j,(direction+1)%4));
			// If you didn't conquer the tile on your right (either with your color or with temp)
			if(adj_free(player,temp,i,j,(direction+1)%4)){
				// If it's not counted yet, count it and mark it ; in case you come here again, you thus won't count it twice
				// Note that we only count the tiles on our right. So when we turn left because there's a tile in front of us, it's counted on the next iteration
				if(neighbour_color >= 'A' && neighbour_color < 'A' + NB_COLORS){
					size += 1;
					set_cell(neighbour_row(i,(direction+1)%4),neighbour_column(j,(direction+1)%4),neighbour_color+32);
				}
				// Then move
				// If there's the edge of the board in front of you, or a case which doesn't belong to you, just turn left
				if(!valid_tile(neighbour_row(i,direction),neighbour_column(j,direction)) || adj_free(player,temp,i,j,direction)){
					direction = (direction-1)%4;
					if(direction < 0){direction += 4;}
				}
				// Else, you can go forward
				else{
					complete_turn = 0;
					i = neighbour_row(i,direction);
					j = neighbour_column(j,direction);
				}
			}
			// If the tile on the right belongs to you, just turn right AND go there
			else{
				complete_turn = 0;
				direction = (direction+1)%4;
				i = neighbour_row(i,direction);
				j = neighbour_column(j,direction);	
			}
		}

		// Updating the stopping condition if you go back to the start
		if(i == start_i && j == start_j){
			start_visited += 1;
		}
	}
	
	// Unmarking all marked tiles
	for(i = 0 ; i < BOARD_SIZE ; i++){
		for(j = 0 ; j < BOARD_SIZE ; j++){
			if(get_cell(i,j) >= 'A' + 32 && get_cell(i,j) < 'A' + NB_COLORS + 32){
				set_cell(i,j,get_cell(i,j)-32);
			}
		}
	}
	return size;
}
