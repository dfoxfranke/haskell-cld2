// Copyright 2014 Daniel Fox Franke
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <cstdlib>
#include <cerrno>
#include <memory>
#include <vector>

#include "compact_lang_det.h"

using namespace std;
using namespace CLD2;

static int
cld2_haskell_shim_impl(const char *buffer,
                       int buffer_length,
                       int is_plain_text,
                       const char *content_language_hint,
                       const char *tld_hint,
                       int encoding_hint,
                       int language_hint,
                       int flags,
                       int *language3,
                       int *percent3,
                       double *normalized_score3,
                       size_t *num_chunks,
                       int **chunk_offsets,
                       unsigned short **chunk_sizes,
                       unsigned short **chunk_langs,
                       int *text_bytes,
                       int *is_reliable) {
  ResultChunkVector resultChunks;
  CLDHints hints;
  Language my_language3[3];
  bool my_is_reliable;
  Language result;

  hints.content_language_hint = content_language_hint;
  hints.tld_hint = tld_hint;
  hints.encoding_hint = encoding_hint;
  hints.language_hint = static_cast<Language>(language_hint);
  
  result = ExtDetectLanguageSummary(buffer, buffer_length,
                                    is_plain_text != 0,
                                    &hints,
                                    flags,
                                    my_language3,
                                    percent3,
                                    normalized_score3,
                                    &resultChunks,
                                    text_bytes,
                                    &my_is_reliable);

  *num_chunks = resultChunks.size();
  *chunk_offsets = 
    static_cast<int*>(calloc(*num_chunks, sizeof (int)));
  *chunk_sizes = static_cast<unsigned short*>
    (calloc(*num_chunks, sizeof (unsigned short)));
  *chunk_langs =
    static_cast<unsigned short*>(calloc(*num_chunks, sizeof (unsigned short)));

  if(*chunk_offsets == NULL || 
     *chunk_sizes == NULL || 
     *chunk_langs == NULL) {
    free(chunk_offsets);
    free(chunk_sizes);
    free(chunk_langs);
    throw bad_alloc();
  }

  for(int i=0; i < *num_chunks; i++) {
    (*chunk_offsets)[i] = resultChunks[i].offset;
    (*chunk_sizes)[i] = resultChunks[i].bytes;
    (*chunk_langs)[i] = resultChunks[i].lang1;
  }

  language3[0] = static_cast<int>(my_language3[0]);
  language3[1] = static_cast<int>(my_language3[1]);
  language3[2] = static_cast<int>(my_language3[2]);
  *is_reliable = static_cast<int>(my_is_reliable);

  return static_cast<int>(result);
}

extern "C" int
cld2_haskell_shim(int *result,
                  const char *buffer,
                  int buffer_length,
                  int is_plain_text,
                  const char *content_language_hint,
                  const char *tld_hint,
                  int encoding_hint,
                  int language_hint,
                  int flags,
                  int *language3,
                  int *percent3,
                  double *normalized_score3,
                  size_t *num_chunks,
                  int **chunk_offsets,
                  unsigned short **chunk_sizes,
                  unsigned short **chunk_langs,
                  int *text_bytes,
                  int *is_reliable) {
  try {
    int my_result = 
      cld2_haskell_shim_impl(buffer,
                             buffer_length,
                             is_plain_text,
                             content_language_hint,
                             tld_hint,
                             encoding_hint,
                             language_hint,
                             flags,
                             language3,
                             percent3,
                             normalized_score3,
                             num_chunks,
                             chunk_offsets,
                             chunk_sizes,
                             chunk_langs,
                             text_bytes,
                             is_reliable);
    *result = my_result;
  } catch(bad_alloc& exc) {
    return ENOMEM;
  } catch(...) {
    return -1;
  }
  
  return 0;
}
