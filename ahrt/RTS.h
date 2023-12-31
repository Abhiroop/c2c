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

#ifndef __RTS_H_
#define __RTS_H_

#include <stdio.h>
#include <stdint.h>

#define TRUE  1
#define FALSE 0


typedef uint64_t UINT;
typedef int64_t  INT;
typedef size_t   WORD;

typedef void* code_label_t;

typedef UINT ptr_t; // represents an STG heap pointer


typedef struct {
  code_label_t standard_entry_code;
  code_label_t evacuation_code;
  code_label_t scavenge_code;
  UINT         other_info; // first 32 bits used to store the total pointer words
                           // next 32 bits store the number of non-pointer words
                           // this is the entire closure layout information Pg 49
} info_table_t;


typedef struct {
  info_table_t *info_table_ptr;
  ptr_t        *heap_ptrs; // array of pointers; GC should follow this
  WORD         *non_ptrs;  // non pointer words
} stg_closure_t;
// we do not use a tag to differentiate an indirection and standard
// closure as the action of entering the closure is the same;
// only the steps to enter are different;



// Note this is a dummy heap that exactly mimics
// the heap of the operational semantics -- basically an
// associative array. We initialise a large enough array
// and the program crashes if this array is full;
// the actual implementation should look like the
// heap_t given further below;
typedef struct {
  stg_closure_t *closures;
} heap_t;

// an actual heap should be like
/* typedef struct { */
/*   WORD *heap_cells; // an array of 64 bit words */
/*   uint8_t *heap_flags; // this could be used to store metadata like mark bit */
/*   // size of heap_cells array == size of heap_flags array */
/* } heap_t */


typedef ptr_t Node; // points to the closure under evaluation

// the Spineless Tagless G-machine
typedef struct {
  heap_t stg_heap;
  ptr_t *stack_a_pointers;
  WORD  *stack_b_non_ptrs;
  Node   stg_node;
  ptr_t  Hp; // unused in the dummy heap
  ptr_t  HLimit; // unused in the dummy heap
  ptr_t  SpA;
  ptr_t  SpB;
} STG;


/*******************/


#endif
