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

//__________Partie II : Composantes fortement connexes : Approche matricielle__________

// Stocke la matrice identité de taille n x n dans m1
void identite(unsigned  int n, bool* m1){
    for(unsigned  int i = 0 ; i < n ; i++){
        for(unsigned  int j = 0 ; j < n ; j++){
            if(i == j){
                m1[id(i,j,n)] = true;
            }
            else{
                m1[id(i,j,n)] = false;
            }
        }
    }
}

// Copie le contenu de la matrice m1 dans la matrice m2 (les deux sont de taille n x n)
void copie(unsigned  int n, bool* m1, bool* m2){
    for(unsigned  int i = 0 ; i < n ; i++){
        for(unsigned  int j = 0 ; j < n ; j++){
            m2[id(i,j,n)] = m1[id(i,j,n)];
        }
    }
}

// Teste si les matrices m1 et m2 sont égales (les deux sont de taille n x n)
bool egale(unsigned  int n, bool* m1, bool* m2){
    for(unsigned  int i = 0 ; i < n ; i++){
        for(unsigned  int j = 0 ; j < n ; j++){
            if(m2[id(i,j,n)] != m1[id(i,j,n)])
                return false;
        }
    }
    return true;
}

// Affiche la matrice de taille n x n donnée
void affiche_matrice(unsigned int n, bool* m1){
    for(unsigned  int i = 0 ; i < n ; i++){
        for(unsigned  int j = 0 ; j < n ; j++){
            if(m1[id(i,j,n)]) printf("1 ");
            else printf("0 ");
        }
        printf("\n");
    }
    printf("\n");
}
// Calcule la somme des matrices (carrées n x n) m1 et m2 et place le résultat dans m3
void somme(unsigned  int n, bool* m1, bool* m2, bool* m3){
    for(unsigned  int i = 0 ; i < n ; i++){
        for(unsigned  int j = 0 ; j < n ; j++){
            m3[id(i,j,n)] = m1[id(i,j,n)] || m2[id(i,j,n)];
        }
    }
}

// Calcule le produit des matrices (carrées n x n) m1 et m2 et place le résultat dans m3
void produit(unsigned  int n, bool* m1, bool* m2, bool* m3){
    for(unsigned  int i = 0 ; i < n ; i++){
        for(unsigned  int j = 0 ; j < n ; j++){
            m3[id(i,j,n)] = false;
            for(unsigned  int k = 0 ; k < n ; k++){
                m3[id(i,j,n)] = m3[id(i,j,n)] || (m1[id(i,k,n)] & m2[id(k,j,n)]);
            }
        }
    }
}

// Calcule la puissance p de la matrice (carrée n x n) m1 et place le résultat dans m2
void puissance(unsigned  int n, bool* m1, unsigned  int p, bool* m2){
    identite(n,m2);
    for(unsigned int m = 1 ; m <= p ; m++){
        bool* m3 = malloc(n*n*(sizeof(bool)));
        produit(n,m1,m2,m3);
        copie(n,m3,m2);
        free(m3);
    }
}

void chemins(graph* g, bool* m){
    unsigned int n = g->N;
    
    // b_prec sera la matrice B_(k-1) à l'itération k
    bool* b_prec = malloc(n*n*(sizeof(bool)));
    identite(n,b_prec);
    
    // m sera la matrice B_k à l'itération k. au début, on a I + B
    somme(n,b_prec,g->adj,m);
    
    // Tant que B_(k-1) != B_k
    int k = 1;
    while(!egale(n,b_prec,m)){
        k++;
        bool* curr_pow = malloc(n*n*(sizeof(bool)));
        
        // on calcule B^k
        puissance(n, g->adj, k, curr_pow);
        
        // on décale les termes de la suite
        copie(n,m,b_prec); // B_(k-1) := m (donc B_k)
        somme(n,b_prec,curr_pow,m); // m := B_k + B_(k+1)
        free(curr_pow);
    }
    free(b_prec);
    
}

void cfc (graph* g, unsigned int* tab){
  unsigned int n = g->N;
  bool* m = malloc(n * n * sizeof(bool));
  chemins(g,m);
  
  for(unsigned int i = 0 ; i < n ; i++){
      if(tab[i] == g->N){
          tab[i] = i;
          for(unsigned int j = 0 ; j < n ; j++){
              if(m[id(i,j,n)] && m[id(j,i,n)]) tab[j] = i;
          }
      }
  }
  
  free(m);  
}

//__________Main__________
int main(int argc, char** argv) {
    
  graph* g = malloc(sizeof(graph));
  create_graph(g, 8);
  
  set_edge(g,1,0,true);
  set_edge(g,0,4,true);
  set_edge(g,4,1,true);
  set_edge(g,5,4,true);
  set_edge(g,5,1,true);
  set_edge(g,5,6,true);
  set_edge(g,6,5,true);
  set_edge(g,6,2,true);
  set_edge(g,2,1,true);
  set_edge(g,3,2,true);
  set_edge(g,2,3,true);
  set_edge(g,7,3,true);
  set_edge(g,7,6,true);
  
  unsigned int* t = malloc(g->N * sizeof(unsigned int));
  for(unsigned int i = 0 ; i < g->N ; i++){
      t[i] = g->N; // on initialise avec un sommet inexistant
  }
  
  cfc(g,t);
  for(unsigned int i = 0 ; i < g->N ; i++){
      printf("%u : %u\n", i, t[i]);
  } 
  
  release_graph(g);
  free(t);
  
  return 0;
}
