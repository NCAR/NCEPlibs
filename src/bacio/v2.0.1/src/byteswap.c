/*--------------------------------------------------------------------*/
/* Documentation block                                                */
/*                                                                    */
/*   byteswap: to reverse the order of a sequence of bytes. it takes  */
/*     an data array, the number of bytes to swap for each data       */
/*     and the number of data elements in the data array to swap,     */
/*     then swaps each data element, and returns with swapped data    */
/*   Aug 2012      Jun Wang                                           */
/*   input :                                                          */
/*     char* data: input data array                                   */
/*     int (or long long int) *nbyte: the number of bytes to swap     */
/*                 for 4 byte data, the number of bytes is 4          */
/*                 for 8 byte data, the number of bytes is 8          */
/*                  the maximal number of bytes to swap is 256        */
/*     int *nnum:  the number of data elements to swap                */
/*   output :                                                         */
/*                                                                    */
/*     char* data: swappted  data array                               */
/*--------------------------------------------------------------------*/

#ifdef LINUX
  void byteswap_
         (char *data, int *nbyte, int *nnum) {
#endif
#ifdef MACOSX
  void byteswap_
         (char *data, int *nbyte, int *nnum) {
#endif
#ifdef IBM4
  void byteswap
         (char *data, int *nbyte, int *nnum) {
#endif
#ifdef IBM8
  void byteswap
         (char *data, long long int *nbyte, long long int *nnum) {
#endif
  int  i, j;
  char swap[256];
  int  nb=*nbyte;
  int  nn=*nnum;


  for (j=0; j<nn; j++) {

    for (i=0; i<nb; i++) swap[i] = data[j*nb+i];

    for (i=0; i<nb; i++) data[j*nb+i] = swap[nb-i-1];

  }

}

