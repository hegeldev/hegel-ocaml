#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

CAMLprim value caml_unsetenv(value v_name) {
  CAMLparam1(v_name);
  unsetenv(String_val(v_name));
  CAMLreturn(Val_unit);
}
