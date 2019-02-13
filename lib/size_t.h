#ifndef EWAH_SIZE_T
#define EWAH_SIZE_T

#if defined(NO_SIZE_T)
typedef unsigned NO_SIZE_T size_t;
#elif defined(STDC)
#  include <stddef.h>
#elif defined(WIN32)
#  include <BaseTsd.h>
typedef SIZE_T size_t;
#else
typedef unsigned long size_t;
#endif

#endif
