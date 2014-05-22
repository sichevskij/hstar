#include <fstream>
#include <iostream>
#include <sys/time.h>
#include <vector>
#include <errno.h>

#include <ANN/ANN.h>

#ifdef __cplusplus
extern "C" {  // only need to export C interface if
              // used by C++ source code
#endif

using namespace std;

double TimeDiff(timeval t1, timeval t2);

void create_kdtree(const char* filename, int kdtree_dim, int n, float points[])
{
    ANNpointArray dataPts = annAllocPts(n, kdtree_dim);

    for(int i=0; i < n; i++) {
        for(int j=0; j < kdtree_dim; j++ ) {
            dataPts[i][j] = points[kdtree_dim*i + j];
        }
    }

    timeval t1, t2;

    gettimeofday(&t1, NULL);

    ANNkd_tree* kdTree = new ANNkd_tree(dataPts, n, kdtree_dim);

    gettimeofday(&t2, NULL);

    double create_time = TimeDiff(t1,t2);

    ofstream of;
    of.open(filename, ios::out);
    kdTree->Dump(ANNtrue, of);
    of.close();

    fprintf(stderr,"Количество точек в индексе: %d\n", n);
    fprintf(stderr,"Время создания индекса: %g ms\n", create_time);

    delete kdTree;
    annDeallocPts(dataPts);
    annClose();
}

int read_points( float out[][10], const char* filename)
{
   FILE* fin;
   timeval t1, t2;
   int rows;
   float teff, logg, mh, av, rv, th, fuv,nuv, u,g,r,i,z, j,h,k;

   gettimeofday(&t1, NULL);

   fin = fopen(filename,"r");

   if (!fin) {
     fprintf(stderr,"read_points: Cannot open input file '%s'.\n", filename);
     exit(1);
   }

   errno = 0;
   int sn = fscanf(fin,"%d", &rows);
   if (sn != 1) {
      if (errno != 0) {
         perror("scanf");
      } else {
         fprintf(stderr, "read_points: No matching characters when reading the number of rows.\n");
      }
      exit(1);
   }

   int n;
   for (n = 0; n < rows; ++n) {

       errno = 0;

       int sn;

       sn = fscanf(fin,"%g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g" ,&teff, &logg, &mh, &av, &rv, &th, &fuv,&nuv, &u,&g,&r,&i,&z, &j,&h,&k);

       if (sn == 16) {
           out[n][0] = fuv;
           out[n][1] = nuv;
           out[n][2] = u;
           out[n][3] = g;
           out[n][4] = r;
           out[n][5] = i;
           out[n][6] = z;
           out[n][7] = j;
           out[n][8] = h;
           out[n][9] = k;
       }

       if (sn == EOF) {
         fprintf(stderr, "read_points: Unexpected reaching of EOF.\n");
         n = rows;
       } else if (errno != 0) {
         perror("scanf");
       } else { 
       }
   }

   fclose(fin);

   return n;
}

ANNkd_tree* load_kdtree(const char* filename)
{

  ANNkd_tree* kdTree;

  ifstream dump;

  dump.open(filename, ios::in);

  kdTree = new ANNkd_tree(dump);

  dump.close();

  return kdTree;

}

void search_kdtree(ANNkd_tree* kdTree, int nq, float queries[], int out[], float dist_sq[])
{
  int k = 1;
  int kdtree_dim = kdTree->theDim();

  ANNidxArray nnIdx = new ANNidx[k];
  ANNdistArray dists = new ANNdist[k];
  ANNpoint queryPt = annAllocPt(kdtree_dim);

  for(int i=0; i < nq; i++) {
    for(int j=0; j < kdtree_dim; j++) {
      queryPt[j] = queries[kdtree_dim*i + j];

//      printf("%f, ", queryPt[j]);
    }
//    printf("\n");

    kdTree->annkSearch(queryPt, 1, nnIdx, dists);

    out[i] = nnIdx[0];
    dist_sq[i] = dists[0];
  }

  delete [] nnIdx;
  delete [] dists;
}

void search_kdtree_v2(float data[][5], ANNkd_tree* kdTree, int nq, float queries[], float out[][5], float dist_sq[])
{
  int k = 1;
  int kdtree_dim = kdTree->theDim();

  ANNidxArray nnIdx = new ANNidx[k];
  ANNdistArray dists = new ANNdist[k];
  ANNpoint queryPt = annAllocPt(kdtree_dim);

  for(int i=0; i < nq; i++) {
    for(int j=0; j < kdtree_dim; j++) {
      queryPt[j] = queries[kdtree_dim*i + j];

//      printf("%f, ", queryPt[j]);
    }
//    printf("\n");

    kdTree->annkSearch(queryPt, 1, nnIdx, dists);

    for (int k=0; k < 5; k++) {
       out[i][k] = data[nnIdx[0]][k];
    } 
    dist_sq[i] = dists[0];
  }

  delete [] nnIdx;
  delete [] dists;
}

double TimeDiff(timeval t1, timeval t2)
{
    double t;
    t = (t2.tv_sec - t1.tv_sec) * 1000.0;      // sec to ms
    t += (t2.tv_usec - t1.tv_usec) / 1000.0;   // us to ms

    return t;
}

#ifdef __cplusplus
}
#endif
