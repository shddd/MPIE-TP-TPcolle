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
}

// Création d'un graphe à nb sommets à un pointeur graph donné
void create_graph(graph *g, unsigned int nb) {
  // Ecrivez ici votre code pour initialiser un graphe à nb sommets
}

// Libération de la mémoire
void release_graph(graph *g) {
  // Ecrivez ici le code nécessaire pour libérer la mémoire
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


//__________Main__________
int main(int argc, char** argv) {
    
  graph* g = malloc(sizeof(graph));
  create_graph(g, 8);
  print_graph(*g);
  release_graph(g);
  
  return 0;
}
