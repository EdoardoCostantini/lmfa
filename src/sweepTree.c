#include <stdlib.h>
#include <R.h>
#include <stdio.h>
#include <math.h>
#include <R_ext/Utils.h>
#include <Rmath.h>

void addOneToVector(int *n, double *vector)
{
  for (int i = 0; i < *n; ++i)
    vector[i] += 1.0;
}

/* copy covar matrix function */
void copyCovarMatrix(double *x, double *y, int k)
{
  int i;
  for (i = 0; i < (k * (k + 1)) / 2; i++)
    x[i] = y[i];
  return;
}

/* perform sweep */
void VSWP(
    double *v,
    int i,
    int n)
{

  int j, k, m, N;
  double *x = NULL;
  double *y = NULL;
  double *z = NULL;
  double xii;
  int *row;

  N = (n * (n + 1)) / 2;

  if (i == 0)
  {
    xii = v[0];
  }
  else
  {
    xii = v[i * n - (i * (i - 1)) / 2];
  }

  if (xii < 10e-20)
  {
    // Rprintf("WARNING: Sweeping singular matrix.\n");
    return;
  }

  // allocate if needed
  if (x == NULL)
  {
    x = calloc(N, sizeof(double));
    y = calloc(N, sizeof(double));
    z = calloc(n, sizeof(double));
    row = calloc(n, sizeof(int));

    // reduce row calculations
    row[0] = 0;
    for (j = 1; j < n; j++)
      row[j] += row[j - 1] + n - j + 1;
  }

  xii = 1 / xii;

  /* create easy to access index */
  for (j = 0; j < i; j++)
  {
    z[j] = v[row[j] - j + i];
  }
  for (; j < n; j++)
  {
    z[j] = v[row[i] - i + j];
  }

  /* fill the rest of the buffer */
  m = 0;
  for (j = 0; j < n; j++)
  {
    for (k = j; k < n; k++)
    {
      x[m] = z[k] * xii;
      m++;
    }
  }

  /* create a second buffer and multiply by xii */
  m = 0;
  for (j = 0; j < n; j++)
  {
    for (k = j; k < n; k++)
    {
      y[m] = z[j];
      m++;
    }
  }

  /* perform my math */
  for (j = 0; j < N; j++)
  {
    v[j] = v[j] - y[j] * x[j];
  };

  /* for the case of i == j replace x[j]*xii by xii */
  for (j = 0; j < i; j++)
  {
    v[row[j] - j + i] = x[j];
  }
  for (; j < n; j++)
  {
    v[row[i] - i + j] = x[j];
  }
  v[row[i]] = -1 * xii;

  // free needed
  free(z);
  free(x);
  free(y);
  free(row);

  return;
}

/* perform reverse sweep */
void VRevSWP(
    double *v,
    int i,
    int n)
{

  int j, k, m, N;
  double *x = NULL;
  double *y = NULL;
  double *z = NULL;
  double xii;
  int *row;

  N = (n * (n + 1)) / 2;

  if (i == 0)
  {
    xii = v[0];
  }
  else
  {
    xii = v[i * n - (i * (i - 1)) / 2];
  }

  if (fabs(xii) < 10e-20)
  {
    Rprintf("WARNING: Reverse sweeping singular matrix.\n");
    return;
  }

  // allocate if needed
  if (x == NULL)
  {
    x = calloc(N, sizeof(double));
    y = calloc(N, sizeof(double));
    z = calloc(N, sizeof(double));
    row = calloc(n, sizeof(int));

    // reduce row calculations
    row[0] = 0;
    for (j = 1; j < n; j++)
      row[j] += row[j - 1] + n - j + 1;
  }

  xii = 1 / xii;

  /* create easy to access index */
  for (j = 0; j < i; j++)
  {
    z[j] = v[row[j] - j + i];
  }
  for (; j < n; j++)
  {
    z[j] = v[row[i] - i + j];
  }

  /* fill the rest of the buffer */
  m = 0;
  for (j = 0; j < n; j++)
  {
    for (k = j; k < n; k++)
    {
      x[m] = z[k] * xii;
      m++;
    }
  }

  /* create a second buffer and multiply by xii */
  m = 0;
  for (j = 0; j < n; j++)
  {
    for (k = j; k < n; k++)
    {
      y[m] = z[j];
      m++;
    }
  }

  /* perform my math */
  for (j = 0; j < N; j++)
    v[j] = v[j] - y[j] * x[j];

  /* for the case of i == j replace x[j]*xii by xii */
  for (j = 0; j < i; j++)
  {
    v[row[j] - j + i] = -1 * x[j];
  }
  for (; j < n; j++)
  {
    v[row[i] - i + j] = -1 * x[j];
  }
  v[row[i]] = -1 * xii;

  // free needed
  free(z);
  free(x);
  free(y);
  free(row);

  return;
}

/* R interface to perform reverse sweep */
void RVSWP(
    double *v,
    int *i,
    int *n,
    int *p)
{
  int j;
  for (j = 0; j < *p; j++)
    VSWP(v, i[j], *n);
  return;
}

/* R interface to perform reverse sweep */
void RVRevSWP(
    double *v,
    int *i,
    int *n,
    int *p)
{
  int j;
  for (j = 0; j < *p; j++)
    VRevSWP(v, i[j], *n);
  return;
}
