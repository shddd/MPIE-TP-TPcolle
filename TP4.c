#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>
#include <math.h>

struct node {
  unsigned int d;
  unsigned int f;
  unsigned int parent;
  unsigned int status;
} typedef node;

const unsigned int NONVU = 0;
const unsigned int EXPLORATION = 1;
const unsigned int VU = 2;

struct digraph {
  unsigned int S;
  node *nodes;
  bool* adj;
} typedef digraph;

bool has_edge(const digraph *graph, unsigned int s, unsigned int t) {
  return graph->adj[s * graph->S + t];
}

void set_edge(digraph *graph, unsigned int s, unsigned int t, bool w) {
  graph->adj[s * graph->S + t] = w;
}

void init_graph(digraph *graph, unsigned int size) {
  int N = size * size; // Nombre de sommets
  graph->S = N;
  graph->nodes = malloc(N * sizeof(node));
  graph->adj = malloc(N * N * sizeof(bool));
  for (unsigned int s = 0; s < N; s++) { // Chaque sommet
    for (unsigned int t = 0; t < N; t++) {
      set_edge(graph, s, t, false); // Pas d'arc
    }
  }
  for (unsigned int s = 0; s < N; s++) { // Chaque sommet
    unsigned int x, y;
    x = s / size;
    y = s % size;
    if (y < size - 1 && s + 1 < N) {
      if (rand()% 2 == 0) {
        set_edge(graph, s, s + 1, true);
      } else {
        set_edge(graph, s + 1, s, true);
      }
    }
    if (s + size < N) {
      if (rand()% 2 == 0) {
        set_edge(graph, s, s + size, true);
      } else {
        set_edge(graph, s + size, s, true);
      }
    }
  }
}

void release_graph(digraph *graph) {
  free(graph->nodes);
  free(graph->adj);
}

void visit(digraph *graph, unsigned int s, unsigned int *tmp) {
  *tmp = *tmp + 1;
  //printf("# Visit: %d\n", s);
  graph->nodes[s].d = *tmp;
  graph->nodes[s].status = EXPLORATION;
  for (unsigned int t = 0; t < graph->S; t++) {
    if (has_edge(graph, s, t) && graph->nodes[t].status == NONVU) {
      graph->nodes[t].parent = s;
      visit(graph, t, tmp);
    }
  }
  *tmp = *tmp + 1;
  graph->nodes[s].f = *tmp;
  graph->nodes[s].status = VU;
}

void depth_search(digraph *graph, unsigned int *order) {
  printf("# Depth First Search\n");
  for (unsigned int i = 0; i < graph->S; i++) { // Chaque sommet
    graph->nodes[i].status = NONVU;
    graph->nodes[i].parent = i;
  }
  unsigned int tmp = 0;
  for (unsigned int i = 0; i < graph->S; i++) { // Chaque sommet
    unsigned int s = order[i];
    if (graph->nodes[s].status == NONVU) {
      visit(graph, s, &tmp);
    }
  }
}

digraph *transpose(const digraph *graph) {
  digraph *tgraph = malloc(sizeof(digraph));
  tgraph->nodes = malloc(graph->S * sizeof(node));
  tgraph->adj = malloc(graph->S * graph->S * sizeof(bool));
  tgraph->S = graph->S;
  for (unsigned int i = 0; i < graph->S; i++) { // Chaque sommet
    for (unsigned int j = 0; j < graph->S; j++) {
      set_edge(tgraph, i, j, has_edge(graph, j, i));
    }
  }
  return tgraph;
}

void sort(const digraph *graph, unsigned int *order, int from, int to) {
  if (to - from <= 1) {
    return;
  }
  const unsigned int pivot = graph->nodes[order[from]].f;
  int next_max = from + 1;
  int next_min = to;
  // On positionne correctement les éléments avant/après le pivot
  while (next_min >= next_max) {
    int s = order[next_max];
    // Si on trouve un sommet de date de fin plus grande que le pivot
    // (donc qui doit être placé avant), on continue plus loin dans le tableau
    if (graph->nodes[s].f >= pivot) {
      next_max++;
    }
    // Sinon, il doit être placé après ; on le met à la fin du tableau, à la dernière "position libre" 
    else {
      int buf = order[next_max];
      order[next_max] = order[next_min]; 
      order[next_min] = buf;
      next_min--;
    }
  }
  // On place le pivot
  unsigned int buf = order[from];
  order[from] = order[next_min]; 
  order[next_min] = buf;
  // On trie récursivement chaque partie du tableau
  if (next_min > 0)
    sort(graph, order, from, next_min - 1);
  sort(graph, order, next_max, to);
}

unsigned int get_root(struct digraph* graph, unsigned int i){
  if(graph->nodes[i].parent == i){
    return i;
  }
  else{
    return get_root(graph, graph->nodes[i].parent);
  }
}

void to_neato(const digraph graph) {
  // Define the theme for the graph
  printf("digraph G {\n  node [shape = point colorscheme=paired12 width=0.4]\n  edge [colorscheme=paired12 penwidth=4]\n");
  unsigned int n = sqrt(graph.S);
  for (unsigned int  i = 0; i < graph.S; i++) {
    unsigned int x, y, color;
    x = i % n;
    y = i / n;
    color = get_root(&graph, i) % 12 + 1;
    printf("  %d[pos=\"%d,%d!\" label=\"%d / %d\" color=\"%d\"];\n",
           i, x, y,
           graph.nodes[i].d,
           graph.nodes[i].f,
           color);
    for (unsigned int  j = 0; j < graph.S; j++) {
      if (i != j && has_edge(&graph, i, j)) {
        if (get_root(&graph, i) == get_root(&graph, j)) {
          printf("  edge[color=\"%d\"]\n  %d -> %d;\n", color, i, j);
        } else {
          printf("  edge[color=0]\n  %d -> %d;\n", i, j);
        }
      }
    }
  }
  printf("}\n");
}

void cfc (struct digraph *graph){
  unsigned int* ordre = malloc((graph->S) * sizeof(int));
  for(int i = 0; i<(graph->S); i++){
    ordre[i] = i;
  }
  
  depth_search(graph, ordre);
  sort(graph, ordre, 0, (graph->S -1));
  struct digraph *grapht = transpose(graph);
  depth_search(grapht, ordre);

  for (int i = 0; i < graph->S; i++) { // Chaque sommet
    graph->nodes[i].parent = grapht->nodes[i].parent;
  }

  release_graph(grapht);
  free(grapht);
  free(ordre);
}

int main(int argc, char** argv) {
  srand(time(NULL));
  digraph graph;
  init_graph(&graph, 10);

  cfc(&graph);

  release_graph(&graph);
  return 0;
}
