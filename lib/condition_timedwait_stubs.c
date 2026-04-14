#include <pthread.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/custom.h>

/* Extract the pthread_cond_t* from an OCaml Condition.t value.
   Layout: custom block whose data area holds a pointer to pthread_cond_t. */
#define Condition_val(v) (* (pthread_cond_t **) Data_custom_val(v))
#define Mutex_val(v)     (* (pthread_mutex_t **) Data_custom_val(v))

/* caml_condition_timedwait(cond, mut, timeout_seconds)
   Returns true if signaled, false if timed out. */
CAMLprim value caml_condition_timedwait(value vcond, value vmut, value vtimeout)
{
  CAMLparam3(vcond, vmut, vtimeout);
  pthread_cond_t *cond = Condition_val(vcond);
  pthread_mutex_t *mut = Mutex_val(vmut);
  double timeout = Double_val(vtimeout);
  struct timespec ts;
  int rc;

  clock_gettime(CLOCK_REALTIME, &ts);
  ts.tv_sec  += (time_t) timeout;
  ts.tv_nsec += (long) (fmod(timeout, 1.0) * 1e9);
  if (ts.tv_nsec >= 1000000000L) {
    ts.tv_sec  += 1;
    ts.tv_nsec -= 1000000000L;
  }

  caml_enter_blocking_section();
  rc = pthread_cond_timedwait(cond, mut, &ts);
  caml_leave_blocking_section();

  CAMLreturn(Val_bool(rc != ETIMEDOUT));
}
