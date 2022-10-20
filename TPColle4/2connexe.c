#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

// Type de graphe orienté
// Les sommets sont des entiers de 0 à N-1
struct graph {
  unsigned int N;
  bool* adj; // Matrice d'adjacence, représenté par un tableau 1D
} typedef graph;

// Identifiant, pour repérer la case de l'arête (i,j) dans la matrice d'adjacence
unsigned int id(unsigned int i, unsigned int j, unsigned int n){
    return i*n+j;
}

// Cherche si l'arête (s,t) existe
bool has_edge(const graph *g, unsigned int s, unsigned int t) {
  return g->adj[id(s,t,g->N)];
}

// Crée l'arête (s,t)
void set_edge(graph *g, unsigned int s, unsigned int t, bool w) {
  g->adj[id(s,t,g->N)] = w;
  g->adj[id(t,s,g->N)] = w;
}

// Création d'un graphe à nb sommets à un pointeur graph donné
void create_graph(graph *g, unsigned int nb) {
  g->N = nb;
  g->adj = malloc(nb * nb * sizeof(bool));
  
  // On n'a aucune arête initialement.
  for (unsigned int s = 0; s < nb; s++) { 
    for (unsigned int t = 0; t < nb; t++) {
        set_edge(g,s,t,false);
    }
  }
}

// Libération de la mémoire
void release_graph(graph *g) {
  free(g->adj);
  free(g);
}

// Affichage du graphe
void print_graph(const graph g){

    // Affichage des arêtes
    for(unsigned int i = 0 ; i < g.N ; i++){
        for (unsigned int j = 0 ; j < g.N ; j++){
            if(has_edge(&g,i,j)){
                printf("%u -> %u\n",i,j);
            }
        }
    }
}

//__________Partie IV : Graphes 2-connexes__________

// Effectue la visite d'un graphe g, en se basant sur un tableau des sommets déjà visités qui sera mis à jour
void visiter(const graph g, int s, bool* visite){
	visite[s] = true;
	for (unsigned int i = 0 ; i < g.N ; i++){
		if(i != s && has_edge(&g,s,i) && !visite[i]){
			visiter(g,i,visite);
		}
	}
}

unsigned int nb_composantes_connexes(const graph g){
    // On crée le tableau de visite et on l'initialise à false
	bool* visite = malloc(g.N * sizeof(bool));
	for(unsigned int i = 0 ; i < g.N ; i++){
	    visite[i] = false;
	}
	
	
	unsigned int nb = 0;
	for(unsigned int i = 0 ; i < g.N ; i++){
	    if(!visite[i]){
	    	visiter(g,i,visite);
	    	nb++;
	    }
	}
	free(visite);
	return nb;
}

bool est_2_connexe(const graph g){
    if(nb_composantes_connexes(g) > 1) return false;
    else{
        for(unsigned int i = 0 ; i < g.N ; i++){
            
            // On crée un nouveau graphe dont on va enlever le sommet i
            graph* new_g = malloc(sizeof(graph));
            new_g->N = g.N;
            create_graph(new_g, g.N);
            
            // En fait, on ne pas l'enlever... juste le déconnecter.
            for(unsigned int j = 0 ; j < g.N ; j++){
                for(unsigned int k = 0 ; k < g.N ; k++){
                    if(has_edge(&g,j,k) && (j != i) && (k != i)){
                        set_edge(new_g,j,k,true);
                    }
                    else{
                        set_edge(new_g,j,k,false);
                    }
                }
            }
            
            // Si on a plus de deux composantes connexes dans le nouveau graphe, c'est forcément le sommet i déconnecté, et le graphe privé de i qui contient au moins deux composantes
            // Alors i est un point d'articulation
            if(nb_composantes_connexes(*new_g) > 2){
                free(new_g);
                return false;
            }
            
            
            free(new_g);
            
        }
        return true;
    }
}

//__________Main__________
int main(int argc, char** argv) {
    
  graph* g = malloc(sizeof(graph));
  create_graph(g, 5);
  
  set_edge(g,0,1,true);
  set_edge(g,0,2,true);
  set_edge(g,2,1,true);
  set_edge(g,2,3,true);
  set_edge(g,2,4,true);
  set_edge(g,3,4,true);
  
  if(est_2_connexe(*g)) printf("true\n");
  else printf("false\n");
  
  
  // On le rend maintenant 2-connexe
  set_edge(g,1,4,true);
  
  if(est_2_connexe(*g)) printf("true\n");
  else printf("false\n");
  
  
  //print_graph(*g);
  release_graph(g);
  
  return 0;
}
