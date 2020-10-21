#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>

#include <string.h>

static mlsize_t mlsize_t_max = -1;

static value caml_array_gather(intnat num_arrays, value arrays[], intnat offsets[], intnat lengths[])
{
  CAMLparamN(arrays, num_arrays);
  value res;                    /* no need to register it as a root */
  mlsize_t i, size, count, pos;
  value * src;

  /* Determine total size and whether result array is an array of floats */
  size = 0;
  for (i = 0; i < num_arrays; i++) {
    if (mlsize_t_max - lengths[i] < size) caml_invalid_argument("Array.gather");
    size += lengths[i];
  }
  if (size == 0) {
    /* If total size = 0, just return empty array */
    res = Atom(0);
  }
  else if (size <= Max_young_wosize) {
    /* Array of values, small enough to fit in young generation.
       We can use memcpy directly. */
    res = caml_alloc_small(size, 0);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      memcpy(&Field(res, pos),
             &Field(arrays[i], offsets[i]),
             lengths[i] * sizeof(value));
      pos += lengths[i];
    }
    CAMLassert(pos == size);
  }
  else if (size > Max_wosize) {
    /* Array of values, too big. */
    caml_invalid_argument("Array.gather");
  } else {
    /* Array of values, must be allocated in old generation and filled
       using caml_initialize. */
    res = caml_alloc_shr(size, 0);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      for (src = &Field(arrays[i], offsets[i]), count = lengths[i];
           count > 0;
           count--, src++, pos++) {
        caml_initialize(&Field(res, pos), *src);
      }
    }
    CAMLassert(pos == size);

    /* Many caml_initialize in a row can create a lot of old-to-young
       refs.  Give the minor GC a chance to run if it needs to.
       Run memprof callbacks for the major allocation. */
    res = caml_process_pending_actions_with_root (res);
  }
  CAMLreturn (res);
}
	
CAMLprim value
caml_array_append_sub(value t1, value off1, value len1,
		      value t2, value off2, value len2)
{
  value arrays[2] = {t1,t2};
  intnat offsets[2] = {Long_val(off1),Long_val(off2)};
  intnat lengths[2] = {Long_val(len1),Long_val(len2)};
  return caml_array_gather(2, arrays, offsets, lengths);
}
