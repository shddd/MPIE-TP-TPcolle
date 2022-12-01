/******************************************************************************

Welcome to GDB Online.
GDB online is an online compiler and debugger tool for C, C++, Python, PHP, Ruby, 
C#, OCaml, VB, Perl, Swift, Prolog, Javascript, Pascal, HTML, CSS, JS
Code, Compile, Run and Debug online from anywhere in world.

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int main()
{
    typedef struct graph {
        int N;
        bool* adj; // matrice d'adjacence du graphce
    } graph;
    
    // Identifiant, pour repérer la case de l'arête (i,j) dans la matrice d'adjacence
    int id(int i, int j, int n){
        return i*n+j;
    }
    
    // Cherche si l'arête (s,t) existe
    bool has_edge(const graph g, int s, int t) {
      return g.adj[id(s,t,g.N)];
    }
    
    // Crée l'arête (s,t) en considérant le graphe comme orienté
    void set_edge(graph *g, unsigned int s, unsigned int t, bool w) {
      g->adj[id(s,t,g->N)] = w;
    }

    // Crée l'arête (s,t) en considérant le graphe comme non-orienté
    void set_both_edge(graph *g, int s, int t, bool w) {
      g->adj[id(s,t,g->N)] = w;
      g->adj[id(t,s,g->N)] = w;
    }
    
    // Création d'un graphe à nb sommets à un pointeur graph donné
    void create_graph(graph *g, int nb) {
      g->N = nb;
      g->adj = malloc(nb * nb * sizeof(bool));
      
      // Chaque sommet est non-coloré, et on n'a aucune arête initialement.
      for (int s = 0; s < nb; s++) { 
        for (int t = 0; t < nb ; t++) {
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
        printf("Sommets :\n");
        // Affichage de chaque sommet avec ses informations du parcours en profondeur (si elles existent)
        for(int i = 0 ; i < g.N ; i++){
            printf("%u : ",i);
        }
        // Affichage des arêtes
        for(int i = 0 ; i < g.N ; i++){
            for (int j = 0 ; j <= i ; j++){
                if(has_edge(g,i,j)){
                    printf("%u -> %u\n",i,j);
                }
            }
        }
    }





    // Ecrivez ici le code de la partie Coloration de graphes
    int varc(graph g, int nb_col, int s, int i){
        return 1+s*nb_col+i;
    }
    
    void clause_une_couleur(graph g, int nb_col, int s){
        for(int k = 1; k<nb_col; k++){
            printf("%d ", varc(g, nb_col, s, k));
        }
        printf("\n");
    }
    
    void cnf_couleur_unique(graph g, int nb_col, int s){
        for(int i = 0; i<nb_col; i++){
            for(int k = 0; k<nb_col; k++){
                if(i != k){
                    printf("-%d ", varc(g, nb_col, s, k));
                }
                else{
                    printf("%d ", varc(g, nb_col, s, k));
                }
            }
            printf("\n");
        }
    }

    // taille en O(k|G|)
    
    void cnf_arete(graph g, int nb_col, int s, unsigned t){
        for(int k = 0; k<nb_col; k++){
            printf("-%d -%d \n", varc(g, nb_col, s, k), varc(g, nb_col, t, k));
        }
    }
    
    void cnf_coloriable(graph g, int nb_col){
        for(int i =0; i<g.N; i++){
            clause_une_couleur(g, nb_col, i);
            cnf_couleur_unique(g, nb_col, i);
            for(int k = 0; k<g.N; k++){
                if(k>i && g.adj[id(i, k, g.N)]){
                    cnf_arete(g, nb_col, i, k);
                }
            }
        }
    }
    
    graph* set_graph1(){
        graph* g = (graph*)malloc(sizeof(graph));
        create_graph(g, 7);
        set_both_edge(g, 0, 5, true);
        set_both_edge(g, 0, 4, true);
        set_both_edge(g, 0, 6, true);
        set_both_edge(g, 1, 5, true);
        set_both_edge(g, 1, 2, true);
        set_both_edge(g, 6, 2, true);
        set_both_edge(g, 3, 2, true);
        set_both_edge(g, 3, 4, true);
        return g;
    }
    
    graph* set_graph2(){
        graph* g = (graph*)malloc(sizeof(graph));
        create_graph(g, 7);
        set_both_edge(g, 0, 2, true);
        set_both_edge(g, 0, 5, true);
        set_both_edge(g, 2, 1, true);
        set_both_edge(g, 1, 4, true);
        set_both_edge(g, 3, 4, true);
        set_both_edge(g, 6, 4, true);
        set_both_edge(g, 3, 5, true);
        set_both_edge(g, 5, 6, true);
        return g;
    }
    
    // Ecrivez ici le code de la partie Chemin hamiltonien
    
    int varh(graph g, int s, int i){
        return 1+s*g.N+i;
    }
    
    void sommet_par_instant(graph g){
        for(int a = 0; a<g.N; a++){
            for(int b = 0; b<g.N; b++){
                if(a != b){
                    printf("%d ", varh(g, a, b));
                }
            }
            printf("\n")
        }
    }

    void sommet_unique_par_instant(graph g){
        for(int z = 0; z<g.N; z++){
            for(int a = 0; a<g.N; a++){
                for(int b = 0; b<g.N; b++){
                    if(a != b){
                        printf("%d ", varh(g, a, b));
                    }
                    if(a == z){
                        printf("-%d ", varh(g, a, b));
                    }
                }
            printf("\n")
            }   
        }
    }
    
    void instant_par_sommet(graph g){
        for(int k = 0; k<g.N; k++){
            for(int j = 0; j<g.N; j++){
                printf("%d ", varh(g, k, j));
            }
            printf("\n");
        }
    }

    
}
