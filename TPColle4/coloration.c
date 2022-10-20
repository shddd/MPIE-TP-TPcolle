#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

// Constantes pour la coloration
const unsigned int NON_COLORE = -1;
const unsigned int ROUGE = 0;
const unsigned int VERT = 1;

// Type de graphe orienté
// Les sommets sont des entiers de 0 à N-1
struct graph {
  unsigned int N;
  int* couleur; // Tableau contenant la couleur de chaque sommet
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

// Crée l'arête (s,t) dans un graphe non-orienté,  doncon crée aussi l'arête (t,s)
void set_both_edge(graph *g, unsigned int s, unsigned int t, bool w) {
  g->adj[id(s,t,g->N)] = w;
  g->adj[id(t,s,g->N)] = w;
}

// Création d'un graphe à nb sommets à un pointeur graph donné
void create_graph(graph *g, unsigned int nb) {
  g->N = nb;
  g->couleur = malloc(nb * sizeof(int));
  g->adj = malloc(nb * nb * sizeof(bool));
  
  // Chaque sommet est non-coloré, et on n'a aucune arête initialement.
  for (unsigned int s = 0; s < nb; s++) { 
    g->couleur[s] = NON_COLORE;
    for (unsigned int t = 0; t < nb; t++) {
        set_edge(g,s,t,false);
    }
  }
}

// Libération de la mémoire
void release_graph(graph *g) {
  free(g->couleur);
  free(g->adj);
  free(g);
}

// Affichage du graphe
void print_graph(const graph g){
    printf("Sommets :\n");
    // Affichage de chaque sommet avec ses informations du parcours en profondeur (si elles existent)
    for(unsigned int i = 0 ; i < g.N ; i++){
        printf("%u : ",i);
        if(g.couleur[i] == NON_COLORE) printf("Non coloré\n");
        else printf("%d\n",g.couleur[i]);
    }
    // Affichage des arêtes
    for(unsigned int i = 0 ; i < g.N ; i++){
        for (unsigned int j = 0 ; j < g.N ; j++){
            if(has_edge(&g,i,j)){
                printf("%u -> %u\n",i,j);
            }
        }
    }
}

//__________Partie I : Graphe biparti et coloration__________

// Cas d'un graphe connexe
bool est_biparti(graph* g){
    g->couleur[0] = ROUGE;
    bool* visite = malloc((g->N)*sizeof(bool)); // indique quels sommets ont été visités
    for(int i = 0 ; i < g->N ; i++){
        visite[i] = false;
    }
    bool est_biparti = true;
    
    // on cherche le premier sommet coloré non-examiné
    for(int i = 0 ; i < g->N ; i++){
        
        // dès qu'on tombe sur un sommet non-examiné et coloré
        if(!visite[i] && (g->couleur[i] != NON_COLORE)){
            visite[i] = true;
            const int COULEUR_OPPOSEE = 1-g->couleur[i];
            
            // on regarde tous ses voisins
            for(int j = 0 ; j < g->N ; j++){
                if(has_edge(g,i,j)){
                    
                    // S'ils sont de la même couleur, le graphe n'est pas biparti
                    if(g->couleur[i] == g->couleur[j]){
                        return false;
                    }
                    // Sinon, on colorie le voisin de l'autre couleur s'il n'était pas colorié
                    else if (g->couleur[j] == NON_COLORE){
                        g->couleur[j] = COULEUR_OPPOSEE;
                    }
                }
                
            }
        i = 0; // on reparcourt la liste des sommets depuis le début
        }
    }
    return true;
}

// Fait une coloration quelconque d'un graphe
void colore(graph* g){
    
    // on cherche à colorier le sommet i
    for(int i = 0 ; i < g->N ; i++){
            bool* couleurs_disponibles = malloc(g->N * sizeof(bool));
            for(int j = 0 ; j < g->N ; j++){
                couleurs_disponibles[j] = true;
            }
            
            // on regarde tous ses voisins
            for(int j = 0 ; j < g->N ; j++){
                // Dès qu'un voisin possède une couleur, on l'enlève des couleurs possibles
                if(has_edge(g,i,j) && (g->couleur[j] != NON_COLORE)){
                    couleurs_disponibles[g->couleur[j]] = false;
                }
            }    
            
            int min_couleur_dispo = 0;
            while(couleurs_disponibles[min_couleur_dispo] == false){
                min_couleur_dispo++;
            }
        
            g->couleur[i] = min_couleur_dispo;
    }
}

//__________Main__________
int main(int argc, char** argv) {
    
  graph* g1 = malloc(sizeof(graph));
  graph* g2 = malloc(sizeof(graph));
  create_graph(g1, 7);
  create_graph(g2, 7);
  
  set_both_edge(g1,0,5,true);
  set_both_edge(g1,5,1,true);
  set_both_edge(g1,1,2,true);
  set_both_edge(g1,0,6,true);
  set_both_edge(g1,6,2,true);
  set_both_edge(g1,0,4,true);
  set_both_edge(g1,4,3,true);
  set_both_edge(g1,3,2,true);

  set_both_edge(g2,0,2,true);
  set_both_edge(g2,2,1,true);
  set_both_edge(g2,1,5,true);
  set_both_edge(g2,5,0,true);
  set_both_edge(g2,5,6,true);
  set_both_edge(g2,6,4,true);
  set_both_edge(g2,4,3,true);
  set_both_edge(g2,3,5,true);
  set_both_edge(g2,4,1,true);

  colore(g2);
  
  print_graph(*g2);
  release_graph(g1);
  release_graph(g2);
  
  return 0;
}
