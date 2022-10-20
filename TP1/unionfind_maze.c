#include "unionfind.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
//#include <bool.h>
#include <time.h>

const int AUCUN_MUR = 0;
const int MUR_OUEST = 1;
const int MUR_SUD = 1 << 1; // En binaire: 10
const int DEUX_MURS = MUR_OUEST | MUR_SUD; // En binaire: 11

int *init_labyrinthe(unsigned int taille) {
  int *labyrinthe = malloc(taille * taille * sizeof(int));
  if (labyrinthe == NULL) {
    perror("malloc init_labyrinthe:");
    exit(1);
  }
  for (unsigned int i = 0; i < taille * taille; i++) {
    labyrinthe[i] = DEUX_MURS;
  }
  return labyrinthe;
}

void afficher_labyrinthe(int *labyrinthe, unsigned int taille) {
  for (unsigned int i = 0; i < taille; i++) {
    printf(" _");
  }
  printf("\n");
  for (unsigned int i = 0; i < taille; i++) {
    printf("|");
    for (unsigned int j = 0; j < taille; j++) {
      if ((labyrinthe[i * taille + j] & MUR_SUD) != 0) {
        printf("_");
      } else {
        printf(" ");
      }
      if ((labyrinthe[i * taille + j] & MUR_OUEST) != 0) {
        printf("|");
      } else {
        printf(" ");
      }
    }
    printf("\n");
  }
}

struct mur {
  int premier;
  int second;
  int vertical; // ou bool
} typedef mur_t;

unsigned int nb_murs(unsigned int taille) {
  return 2 * (taille - 1) * taille;
}

mur_t *liste_murs(unsigned int taille) {
  mur_t *liste = malloc(nb_murs(taille) * sizeof(mur_t));
  if (liste == NULL) {
    perror("malloc liste_murs:");
    exit(1);
  }
  unsigned int k = 0;
  for (unsigned int i = 0; i < taille; i++) {
    for (unsigned int j = 0; j < taille; j++) {
      if (j < taille - 1) { // Il y a un mur Ouest
        liste[k].premier = i * taille + j;
        liste[k].second = i * taille + j + 1;
        liste[k].vertical = 1; // true
        k += 1;
      }
      if (i < taille - 1) { // Il y a un mur Sud
        liste[k].premier = i * taille + j;
        liste[k].second = (i + 1) * taille + j;
        liste[k].vertical = 0; // false
        k += 1;
      }
    }
  }
  return liste;
}

void permuter_murs(mur_t* liste_murs, int premier, int second) {
  mur_t t = liste_murs[premier];
  liste_murs[premier] = liste_murs[second];
  liste_murs[second] = t;
}


void melanger(mur_t *liste_murs, unsigned int taille) {
  for(unsigned int i = 0; i < nb_murs(taille); i++) {
    unsigned int k = rand() % (i + 1);
    permuter_murs(liste_murs, k, i);
  }
}

void construire(int *labyrinthe, unsigned int taille) {
  // Tous les murs
  mur_t *murs = liste_murs(taille);
  // Mélange
  melanger(murs, taille);
  // Construction de la partition
  element_t *partition = init_partition(taille * taille);
  // Lecture séquentielle du mélange
  for (int i = 0; i < nb_murs(taille); i++) {
    int first_cell = murs[i].premier;
    int second_cell = murs[i].second;
    // S'il n'y a pas de chemin entre ces deux cases
    if (find(&(partition[first_cell])) != find(&(partition[second_cell]))) {
      // Fusion des classes
      fusion(&(partition[first_cell]), &(partition[second_cell]));
      // Suppression du mur
      if (murs[i].vertical) { // Mur ouest
        labyrinthe[first_cell] = labyrinthe[first_cell] ^ MUR_OUEST;
      } else { // Mur sud
        labyrinthe[first_cell] = labyrinthe[first_cell] ^ MUR_SUD;
      }
    }
  }
  free_partition(partition);
  free(murs);
}

int main(int argc, char **argv) {
  // Initialisation du générateur de nombre aléatoire
  srand(time(NULL));

  const unsigned int taille = atoi(argv[1]);
  int *labyrinthe = init_labyrinthe(taille);
  afficher_labyrinthe(labyrinthe, taille);
  construire(labyrinthe, taille);
  afficher_labyrinthe(labyrinthe, taille);
  free(labyrinthe);
  return 1;
}
