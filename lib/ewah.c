#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include "size_t.h"

/* XXX(dinosaure): trust on git codebase. */
#define Ptrdiff_val(ba) ((uint8_t*) Caml_ba_data_val (ba))

CAMLprim value
ewah_ptr_diff(value arr0, value arr1)
{
  size_t res = Ptrdiff_val(arr0) - Ptrdiff_val(arr1) ;
  return (Val_int (res)) ;
}
