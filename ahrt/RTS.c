/*******************************************************************************/
/* MIT License                                                                 */

/* Copyright (c) 2023 Abhiroop Sarkar */

/* Permission is hereby granted, free of charge, to any person obtaining a copy */
/* of this software and associated documentation files (the "Software"), to deal */
/* in the Software without restriction, including without limitation the rights */
/* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell */
/* copies of the Software, and to permit persons to whom the Software is */
/* furnished to do so, subject to the following conditions: */

/* The above copyright notice and this permission notice shall be included in all */
/* copies or substantial portions of the Software. */

/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR */
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE */
/* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER */
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, */
/* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE */
/* SOFTWARE. */
/*******************************************************************************/


#include "RTS.h"


static inline code_label_t label_call(code_label_t addr){
  //cast the (void *) to a function pointer
  code_label_t (*castedPtr)() = (code_label_t(*)())addr;
  code_label_t result = castedPtr();
  return result;
}

code_label_t enter(STG *stg_inst, ptr_t cl_addr){
  stg_inst->stg_node = cl_addr;
  code_label_t entry_addr =
    stg_inst->stg_heap.closures[stg_inst->stg_node].
    info_table_ptr->standard_entry_code;
  code_label_t res = label_call(entry_addr);
  return res;
}

void initSTG(STG *stg){
  // initialise the stacks, heap etc
}

int main(){

  STG stg_inst;
  initSTG(&stg_inst);
  printf("Word size - %ld bytes \n",sizeof(size_t));
  return -1;
}


/* Fun with function pointers
 * Emulates Steele's UUO handler technique
 */
/* void *bar(); */

/* //----- */
/* //int *(*j)() = &k; */
/* //----- -- -- */
/* // |    |   v */
/* // v    |   arg type */
/* //return| */
/* //type  | */
/* //      v */
/* //     func ptr */


/* void *foo(){ */
/*   void *(*fptr)() = &bar; */
/*   return(fptr); */
/* } */


/* void *bar(){ */
/*   void *(*fooptr)() = &foo; */
/*   return(fooptr); } */

/* int main(){ */
/*   void *a = foo(); */
/*   printf("bar_addr %p\n", a); */
/*   void *(*castedPtr)() = (void *(*)())a; */
/*   void *b = castedPtr(); */
/*   printf("foo_addr %p\n", b); */
/*   void *(*fooptr)() = &foo; */
/*   printf("confirm foo_addr %p\n", fooptr); */
/*   /\* int (*castedPtr)() = (int (*)())a; *\/ */
/*   /\* printf("%d", castedPtr()); *\/ */
/*   //CodeLabel (*l)() = foo(); */
/*   /\* while(TRUE){ *\/ */
/*   /\*   CodeLabel *cont = cont(); *\/ */
/*   /\* } *\/ */

/*   return -1; */
/* } */



/*   CodeLabel usage  */
/*
void foo() {
  printf("Hello world\n");
}
int main(){
  CodeLabel l = &foo;
  l();
}
*/
