#include <caml/mlvalues.h>

intnat caml_load_int8(int8_t *ptr)
{
  return *ptr;
}

intnat caml_load_int16(int16_t *ptr)
{
  return *ptr;
}

int32_t caml_load_int32(int32_t *ptr)
{
  return *ptr;
}

value caml_store_int8(int8_t *ptr, intnat val)
{
  *ptr = val;
  return Val_unit;
}

value caml_store_int16(int16_t *ptr, intnat val)
{
  *ptr = val;
  return Val_unit;
}

value caml_store_int32(int32_t *ptr, int32_t val)
{
  *ptr = val;
  return Val_unit;
}
