{-# LANGUAGE Trustworthy, DeriveDataTypeable, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-
Copyright 2014 Daniel Fox Franke

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-|
Module : Text.CLD2
License : Apache-2.0
Stability : provisional
Portability: portable

This module provides simple Haskell bindings for Compact Language Detector 2, a language-detection library used by Google Chrome. See <https://code.google.com/p/cld2/>.
-}
module Text.CLD2 (
  detectLanguageDebug, detectLanguage, detectLanguageSimple,
  Language(..),
  Hints(..), defaultHints,
  Encoding(..),
  DebugFlags(..), defaultDebugFlags,
  Result(..),
  Chunk(..)
  ) where

import Control.Exception(AsyncException(..),AssertionFailed(..),mask_,throwIO)
import Data.Bits(Bits(..), (.|.))
import Data.ByteString(ByteString)
import Data.ByteString.Unsafe(unsafeUseAsCStringLen)
import Data.Data(Data)
import Data.Functor((<$>))
import Data.Hashable(Hashable)
import Data.Text(Text)
import Data.Text.Encoding(encodeUtf8)
import Data.Typeable(Typeable)
import Foreign.C.Error(Errno(..), eNOMEM, eOK)
import Foreign.C.String(CString, withCString)
import Foreign.C.Types(CShort(..),CInt(..),CChar(..),CDouble(..),CSize(..))
import Foreign.Ptr(Ptr(..), nullPtr)
import Foreign.Marshal.Alloc(alloca, free)
import Foreign.Marshal.Array(peekArray, allocaArray)
import Foreign.Storable(peek)
import GHC.Generics(Generic)
import System.IO.Unsafe(unsafePerformIO)

-- | An enumeration of all languages recognized by CLD2
data Language =
  Cld2Language_ENGLISH
  | Cld2Language_DANISH
  | Cld2Language_DUTCH
  | Cld2Language_FINNISH
  | Cld2Language_FRENCH
  | Cld2Language_GERMAN
  | Cld2Language_HEBREW
  | Cld2Language_ITALIAN
  | Cld2Language_JAPANESE
  | Cld2Language_KOREAN
  | Cld2Language_NORWEGIAN
  | Cld2Language_POLISH
  | Cld2Language_PORTUGUESE
  | Cld2Language_RUSSIAN
  | Cld2Language_SPANISH
  | Cld2Language_SWEDISH
  | Cld2Language_CHINESE
  | Cld2Language_CZECH
  | Cld2Language_GREEK
  | Cld2Language_ICELANDIC
  | Cld2Language_LATVIAN
  | Cld2Language_LITHUANIAN
  | Cld2Language_ROMANIAN
  | Cld2Language_HUNGARIAN
  | Cld2Language_ESTONIAN
  | Cld2Language_TG_UNKNOWN_LANGUAGE
  | Cld2Language_UNKNOWN_LANGUAGE
  | Cld2Language_BULGARIAN
  | Cld2Language_CROATIAN
  | Cld2Language_SERBIAN
  | Cld2Language_IRISH
  | Cld2Language_GALICIAN
  | Cld2Language_TAGALOG
  | Cld2Language_TURKISH
  | Cld2Language_UKRAINIAN
  | Cld2Language_HINDI
  | Cld2Language_MACEDONIAN
  | Cld2Language_BENGALI
  | Cld2Language_INDONESIAN
  | Cld2Language_LATIN
  | Cld2Language_MALAY
  | Cld2Language_MALAYALAM
  | Cld2Language_WELSH
  | Cld2Language_NEPALI
  | Cld2Language_TELUGU
  | Cld2Language_ALBANIAN
  | Cld2Language_TAMIL
  | Cld2Language_BELARUSIAN
  | Cld2Language_JAVANESE
  | Cld2Language_OCCITAN
  | Cld2Language_URDU
  | Cld2Language_BIHARI
  | Cld2Language_GUJARATI
  | Cld2Language_THAI
  | Cld2Language_ARABIC
  | Cld2Language_CATALAN
  | Cld2Language_ESPERANTO
  | Cld2Language_BASQUE
  | Cld2Language_INTERLINGUA
  | Cld2Language_KANNADA
  | Cld2Language_PUNJABI
  | Cld2Language_SCOTS_GAELIC
  | Cld2Language_SWAHILI
  | Cld2Language_SLOVENIAN
  | Cld2Language_MARATHI
  | Cld2Language_MALTESE
  | Cld2Language_VIETNAMESE
  | Cld2Language_FRISIAN
  | Cld2Language_SLOVAK
  | Cld2Language_CHINESE_T
  | Cld2Language_FAROESE
  | Cld2Language_SUNDANESE
  | Cld2Language_UZBEK
  | Cld2Language_AMHARIC
  | Cld2Language_AZERBAIJANI
  | Cld2Language_GEORGIAN
  | Cld2Language_TIGRINYA
  | Cld2Language_PERSIAN
  | Cld2Language_BOSNIAN
  | Cld2Language_SINHALESE
  | Cld2Language_NORWEGIAN_N
  | Cld2Language_X_81
  | Cld2Language_X_82
  | Cld2Language_XHOSA
  | Cld2Language_ZULU
  | Cld2Language_GUARANI
  | Cld2Language_SESOTHO
  | Cld2Language_TURKMEN
  | Cld2Language_KYRGYZ
  | Cld2Language_BRETON
  | Cld2Language_TWI
  | Cld2Language_YIDDISH
  | Cld2Language_X_92
  | Cld2Language_SOMALI
  | Cld2Language_UIGHUR
  | Cld2Language_KURDISH
  | Cld2Language_MONGOLIAN
  | Cld2Language_ARMENIAN
  | Cld2Language_LAOTHIAN
  | Cld2Language_SINDHI
  | Cld2Language_RHAETO_ROMANCE
  | Cld2Language_AFRIKAANS
  | Cld2Language_LUXEMBOURGISH
  | Cld2Language_BURMESE
  | Cld2Language_KHMER
  | Cld2Language_TIBETAN
  | Cld2Language_DHIVEHI
  | Cld2Language_CHEROKEE
  | Cld2Language_SYRIAC
  | Cld2Language_LIMBU
  | Cld2Language_ORIYA
  | Cld2Language_ASSAMESE
  | Cld2Language_CORSICAN
  | Cld2Language_INTERLINGUE
  | Cld2Language_KAZAKH
  | Cld2Language_LINGALA
  | Cld2Language_X_116
  | Cld2Language_PASHTO
  | Cld2Language_QUECHUA
  | Cld2Language_SHONA
  | Cld2Language_TAJIK
  | Cld2Language_TATAR
  | Cld2Language_TONGA
  | Cld2Language_YORUBA
  | Cld2Language_X_124
  | Cld2Language_X_125
  | Cld2Language_X_126
  | Cld2Language_X_127
  | Cld2Language_MAORI
  | Cld2Language_WOLOF
  | Cld2Language_ABKHAZIAN
  | Cld2Language_AFAR
  | Cld2Language_AYMARA
  | Cld2Language_BASHKIR
  | Cld2Language_BISLAMA
  | Cld2Language_DZONGKHA
  | Cld2Language_FIJIAN
  | Cld2Language_GREENLANDIC
  | Cld2Language_HAUSA
  | Cld2Language_HAITIAN_CREOLE
  | Cld2Language_INUPIAK
  | Cld2Language_INUKTITUT
  | Cld2Language_KASHMIRI
  | Cld2Language_KINYARWANDA
  | Cld2Language_MALAGASY
  | Cld2Language_NAURU
  | Cld2Language_OROMO
  | Cld2Language_RUNDI
  | Cld2Language_SAMOAN
  | Cld2Language_SANGO
  | Cld2Language_SANSKRIT
  | Cld2Language_SISWANT
  | Cld2Language_TSONGA
  | Cld2Language_TSWANA
  | Cld2Language_VOLAPUK
  | Cld2Language_ZHUANG
  | Cld2Language_KHASI
  | Cld2Language_SCOTS
  | Cld2Language_GANDA
  | Cld2Language_MANX
  | Cld2Language_MONTENEGRIN
  | Cld2Language_AKAN
  | Cld2Language_IGBO
  | Cld2Language_MAURITIAN_CREOLE
  | Cld2Language_HAWAIIAN
  | Cld2Language_CEBUANO
  | Cld2Language_EWE
  | Cld2Language_GA
  | Cld2Language_HMONG
  | Cld2Language_KRIO
  | Cld2Language_LOZI
  | Cld2Language_LUBA_LULUA
  | Cld2Language_LUO_KENYA_AND_TANZANIA
  | Cld2Language_NEWARI
  | Cld2Language_NYANJA
  | Cld2Language_OSSETIAN
  | Cld2Language_PAMPANGA
  | Cld2Language_PEDI
  | Cld2Language_RAJASTHANI
  | Cld2Language_SESELWA
  | Cld2Language_TUMBUKA
  | Cld2Language_VENDA
  | Cld2Language_WARAY_PHILIPPINES
  | Cld2Language_X_183
  | Cld2Language_X_184
  | Cld2Language_X_185
  | Cld2Language_X_186
  | Cld2Language_X_187
  | Cld2Language_X_188
  | Cld2Language_X_189
  | Cld2Language_X_190
  | Cld2Language_X_191
  | Cld2Language_X_192
  | Cld2Language_X_193
  | Cld2Language_X_194
  | Cld2Language_X_195
  | Cld2Language_X_196
  | Cld2Language_X_197
  | Cld2Language_X_198
  | Cld2Language_X_199
  | Cld2Language_X_200
  | Cld2Language_X_201
  | Cld2Language_X_202
  | Cld2Language_X_203
  | Cld2Language_X_204
  | Cld2Language_X_205
  | Cld2Language_X_206
  | Cld2Language_X_207
  | Cld2Language_X_208
  | Cld2Language_X_209
  | Cld2Language_X_210
  | Cld2Language_X_211
  | Cld2Language_X_212
  | Cld2Language_X_213
  | Cld2Language_X_214
  | Cld2Language_X_215
  | Cld2Language_X_216
  | Cld2Language_X_217
  | Cld2Language_X_218
  | Cld2Language_X_219
  | Cld2Language_X_220
  | Cld2Language_X_221
  | Cld2Language_X_222
  | Cld2Language_X_223
  | Cld2Language_X_224
  | Cld2Language_X_225
  | Cld2Language_X_226
  | Cld2Language_X_227
  | Cld2Language_X_228
  | Cld2Language_X_229
  | Cld2Language_X_230
  | Cld2Language_X_231
  | Cld2Language_X_232
  | Cld2Language_X_233
  | Cld2Language_X_234
  | Cld2Language_X_235
  | Cld2Language_X_236
  | Cld2Language_X_237
  | Cld2Language_X_238
  | Cld2Language_X_239
  | Cld2Language_X_240
  | Cld2Language_X_241
  | Cld2Language_X_242
  | Cld2Language_X_243
  | Cld2Language_X_244
  | Cld2Language_X_245
  | Cld2Language_X_246
  | Cld2Language_X_247
  | Cld2Language_X_248
  | Cld2Language_X_249
  | Cld2Language_X_250
  | Cld2Language_X_251
  | Cld2Language_X_252
  | Cld2Language_X_253
  | Cld2Language_X_254
  | Cld2Language_X_255
  | Cld2Language_X_256
  | Cld2Language_X_257
  | Cld2Language_X_258
  | Cld2Language_X_259
  | Cld2Language_X_260
  | Cld2Language_X_261
  | Cld2Language_X_262
  | Cld2Language_X_263
  | Cld2Language_X_264
  | Cld2Language_X_265
  | Cld2Language_X_266
  | Cld2Language_X_267
  | Cld2Language_X_268
  | Cld2Language_X_269
  | Cld2Language_X_270
  | Cld2Language_X_271
  | Cld2Language_X_272
  | Cld2Language_X_273
  | Cld2Language_X_274
  | Cld2Language_X_275
  | Cld2Language_X_276
  | Cld2Language_X_277
  | Cld2Language_X_278
  | Cld2Language_X_279
  | Cld2Language_X_280
  | Cld2Language_X_281
  | Cld2Language_X_282
  | Cld2Language_X_283
  | Cld2Language_X_284
  | Cld2Language_X_285
  | Cld2Language_X_286
  | Cld2Language_X_287
  | Cld2Language_X_288
  | Cld2Language_X_289
  | Cld2Language_X_290
  | Cld2Language_X_291
  | Cld2Language_X_292
  | Cld2Language_X_293
  | Cld2Language_X_294
  | Cld2Language_X_295
  | Cld2Language_X_296
  | Cld2Language_X_297
  | Cld2Language_X_298
  | Cld2Language_X_299
  | Cld2Language_X_300
  | Cld2Language_X_301
  | Cld2Language_X_302
  | Cld2Language_X_303
  | Cld2Language_X_304
  | Cld2Language_X_305
  | Cld2Language_X_306
  | Cld2Language_X_307
  | Cld2Language_X_308
  | Cld2Language_X_309
  | Cld2Language_X_310
  | Cld2Language_X_311
  | Cld2Language_X_312
  | Cld2Language_X_313
  | Cld2Language_X_314
  | Cld2Language_X_315
  | Cld2Language_X_316
  | Cld2Language_X_317
  | Cld2Language_X_318
  | Cld2Language_X_319
  | Cld2Language_X_320
  | Cld2Language_X_321
  | Cld2Language_X_322
  | Cld2Language_X_323
  | Cld2Language_X_324
  | Cld2Language_X_325
  | Cld2Language_X_326
  | Cld2Language_X_327
  | Cld2Language_X_328
  | Cld2Language_X_329
  | Cld2Language_X_330
  | Cld2Language_X_331
  | Cld2Language_X_332
  | Cld2Language_X_333
  | Cld2Language_X_334
  | Cld2Language_X_335
  | Cld2Language_X_336
  | Cld2Language_X_337
  | Cld2Language_X_338
  | Cld2Language_X_339
  | Cld2Language_X_340
  | Cld2Language_X_341
  | Cld2Language_X_342
  | Cld2Language_X_343
  | Cld2Language_X_344
  | Cld2Language_X_345
  | Cld2Language_X_346
  | Cld2Language_X_347
  | Cld2Language_X_348
  | Cld2Language_X_349
  | Cld2Language_X_350
  | Cld2Language_X_351
  | Cld2Language_X_352
  | Cld2Language_X_353
  | Cld2Language_X_354
  | Cld2Language_X_355
  | Cld2Language_X_356
  | Cld2Language_X_357
  | Cld2Language_X_358
  | Cld2Language_X_359
  | Cld2Language_X_360
  | Cld2Language_X_361
  | Cld2Language_X_362
  | Cld2Language_X_363
  | Cld2Language_X_364
  | Cld2Language_X_365
  | Cld2Language_X_366
  | Cld2Language_X_367
  | Cld2Language_X_368
  | Cld2Language_X_369
  | Cld2Language_X_370
  | Cld2Language_X_371
  | Cld2Language_X_372
  | Cld2Language_X_373
  | Cld2Language_X_374
  | Cld2Language_X_375
  | Cld2Language_X_376
  | Cld2Language_X_377
  | Cld2Language_X_378
  | Cld2Language_X_379
  | Cld2Language_X_380
  | Cld2Language_X_381
  | Cld2Language_X_382
  | Cld2Language_X_383
  | Cld2Language_X_384
  | Cld2Language_X_385
  | Cld2Language_X_386
  | Cld2Language_X_387
  | Cld2Language_X_388
  | Cld2Language_X_389
  | Cld2Language_X_390
  | Cld2Language_X_391
  | Cld2Language_X_392
  | Cld2Language_X_393
  | Cld2Language_X_394
  | Cld2Language_X_395
  | Cld2Language_X_396
  | Cld2Language_X_397
  | Cld2Language_X_398
  | Cld2Language_X_399
  | Cld2Language_X_400
  | Cld2Language_X_401
  | Cld2Language_X_402
  | Cld2Language_X_403
  | Cld2Language_X_404
  | Cld2Language_X_405
  | Cld2Language_X_406
  | Cld2Language_X_407
  | Cld2Language_X_408
  | Cld2Language_X_409
  | Cld2Language_X_410
  | Cld2Language_X_411
  | Cld2Language_X_412
  | Cld2Language_X_413
  | Cld2Language_X_414
  | Cld2Language_X_415
  | Cld2Language_X_416
  | Cld2Language_X_417
  | Cld2Language_X_418
  | Cld2Language_X_419
  | Cld2Language_X_420
  | Cld2Language_X_421
  | Cld2Language_X_422
  | Cld2Language_X_423
  | Cld2Language_X_424
  | Cld2Language_X_425
  | Cld2Language_X_426
  | Cld2Language_X_427
  | Cld2Language_X_428
  | Cld2Language_X_429
  | Cld2Language_X_430
  | Cld2Language_X_431
  | Cld2Language_X_432
  | Cld2Language_X_433
  | Cld2Language_X_434
  | Cld2Language_X_435
  | Cld2Language_X_436
  | Cld2Language_X_437
  | Cld2Language_X_438
  | Cld2Language_X_439
  | Cld2Language_X_440
  | Cld2Language_X_441
  | Cld2Language_X_442
  | Cld2Language_X_443
  | Cld2Language_X_444
  | Cld2Language_X_445
  | Cld2Language_X_446
  | Cld2Language_X_447
  | Cld2Language_X_448
  | Cld2Language_X_449
  | Cld2Language_X_450
  | Cld2Language_X_451
  | Cld2Language_X_452
  | Cld2Language_X_453
  | Cld2Language_X_454
  | Cld2Language_X_455
  | Cld2Language_X_456
  | Cld2Language_X_457
  | Cld2Language_X_458
  | Cld2Language_X_459
  | Cld2Language_X_460
  | Cld2Language_X_461
  | Cld2Language_X_462
  | Cld2Language_X_463
  | Cld2Language_X_464
  | Cld2Language_X_465
  | Cld2Language_X_466
  | Cld2Language_X_467
  | Cld2Language_X_468
  | Cld2Language_X_469
  | Cld2Language_X_470
  | Cld2Language_X_471
  | Cld2Language_X_472
  | Cld2Language_X_473
  | Cld2Language_X_474
  | Cld2Language_X_475
  | Cld2Language_X_476
  | Cld2Language_X_477
  | Cld2Language_X_478
  | Cld2Language_X_479
  | Cld2Language_X_480
  | Cld2Language_X_481
  | Cld2Language_X_482
  | Cld2Language_X_483
  | Cld2Language_X_484
  | Cld2Language_X_485
  | Cld2Language_X_486
  | Cld2Language_X_487
  | Cld2Language_X_488
  | Cld2Language_X_489
  | Cld2Language_X_490
  | Cld2Language_X_491
  | Cld2Language_X_492
  | Cld2Language_X_493
  | Cld2Language_X_494
  | Cld2Language_X_495
  | Cld2Language_X_496
  | Cld2Language_X_497
  | Cld2Language_X_498
  | Cld2Language_X_499
  | Cld2Language_X_500
  | Cld2Language_X_501
  | Cld2Language_X_502
  | Cld2Language_X_503
  | Cld2Language_X_504
  | Cld2Language_X_505
  | Cld2Language_NDEBELE
  | Cld2Language_X_BORK_BORK_BORK
  | Cld2Language_X_PIG_LATIN
  | Cld2Language_X_HACKER
  | Cld2Language_X_KLINGON
  | Cld2Language_X_ELMER_FUDD
  | Cld2Language_X_Common
  | Cld2Language_X_Latin
  | Cld2Language_X_Greek
  | Cld2Language_X_Cyrillic
  | Cld2Language_X_Armenian
  | Cld2Language_X_Hebrew
  | Cld2Language_X_Arabic
  | Cld2Language_X_Syriac
  | Cld2Language_X_Thaana
  | Cld2Language_X_Devanagari
  | Cld2Language_X_Bengali
  | Cld2Language_X_Gurmukhi
  | Cld2Language_X_Gujarati
  | Cld2Language_X_Oriya
  | Cld2Language_X_Tamil
  | Cld2Language_X_Telugu
  | Cld2Language_X_Kannada
  | Cld2Language_X_Malayalam
  | Cld2Language_X_Sinhala
  | Cld2Language_X_Thai
  | Cld2Language_X_Lao
  | Cld2Language_X_Tibetan
  | Cld2Language_X_Myanmar
  | Cld2Language_X_Georgian
  | Cld2Language_X_Hangul
  | Cld2Language_X_Ethiopic
  | Cld2Language_X_Cherokee
  | Cld2Language_X_Canadian_Aboriginal
  | Cld2Language_X_Ogham
  | Cld2Language_X_Runic
  | Cld2Language_X_Khmer
  | Cld2Language_X_Mongolian
  | Cld2Language_X_Hiragana
  | Cld2Language_X_Katakana
  | Cld2Language_X_Bopomofo
  | Cld2Language_X_Han
  | Cld2Language_X_Yi
  | Cld2Language_X_Old_Italic
  | Cld2Language_X_Gothic
  | Cld2Language_X_Deseret
  | Cld2Language_X_Inherited
  | Cld2Language_X_Tagalog
  | Cld2Language_X_Hanunoo
  | Cld2Language_X_Buhid
  | Cld2Language_X_Tagbanwa
  | Cld2Language_X_Limbu
  | Cld2Language_X_Tai_Le
  | Cld2Language_X_Linear_B
  | Cld2Language_X_Ugaritic
  | Cld2Language_X_Shavian
  | Cld2Language_X_Osmanya
  | Cld2Language_X_Cypriot
  | Cld2Language_X_Braille
  | Cld2Language_X_Buginese
  | Cld2Language_X_Coptic
  | Cld2Language_X_New_Tai_Lue
  | Cld2Language_X_Glagolitic
  | Cld2Language_X_Tifinagh
  | Cld2Language_X_Syloti_Nagri
  | Cld2Language_X_Old_Persian
  | Cld2Language_X_Kharoshthi
  | Cld2Language_X_Balinese
  | Cld2Language_X_Cuneiform
  | Cld2Language_X_Phoenician
  | Cld2Language_X_Phags_Pa
  | Cld2Language_X_Nko
  | Cld2Language_X_Sundanese
  | Cld2Language_X_Lepcha
  | Cld2Language_X_Ol_Chiki
  | Cld2Language_X_Vai
  | Cld2Language_X_Saurashtra
  | Cld2Language_X_Kayah_Li
  | Cld2Language_X_Rejang
  | Cld2Language_X_Lycian
  | Cld2Language_X_Carian
  | Cld2Language_X_Lydian
  | Cld2Language_X_Cham
  | Cld2Language_X_Tai_Tham
  | Cld2Language_X_Tai_Viet
  | Cld2Language_X_Avestan
  | Cld2Language_X_Egyptian_Hieroglyphs
  | Cld2Language_X_Samaritan
  | Cld2Language_X_Lisu
  | Cld2Language_X_Bamum
  | Cld2Language_X_Javanese
  | Cld2Language_X_Meetei_Mayek
  | Cld2Language_X_Imperial_Aramaic
  | Cld2Language_X_Old_South_Arabian
  | Cld2Language_X_Inscriptional_Parthian
  | Cld2Language_X_Inscriptional_Pahlavi
  | Cld2Language_X_Old_Turkic
  | Cld2Language_X_Kaithi
  | Cld2Language_X_Batak
  | Cld2Language_X_Brahmi
  | Cld2Language_X_Mandaic
  | Cld2Language_X_Chakma
  | Cld2Language_X_Meroitic_Cursive
  | Cld2Language_X_Meroitic_Hieroglyphs
  | Cld2Language_X_Miao
  | Cld2Language_X_Sharada
  | Cld2Language_X_Sora_Sompeng
  | Cld2Language_X_Takri
  deriving (Eq,Ord,Show,Bounded,Enum,Typeable,Data)

-- | An enumeration of character encodings which can be included in 'Hints'
data Encoding =
  Cld2Encoding_ISO_8859_1
  | Cld2Encoding_ISO_8859_2
  | Cld2Encoding_ISO_8859_3
  | Cld2Encoding_ISO_8859_4
  | Cld2Encoding_ISO_8859_5
  | Cld2Encoding_ISO_8859_6
  | Cld2Encoding_ISO_8859_7
  | Cld2Encoding_ISO_8859_8
  | Cld2Encoding_ISO_8859_9
  | Cld2Encoding_ISO_8859_10
  | Cld2Encoding_JAPANESE_EUC_JP
  | Cld2Encoding_JAPANESE_SHIFT_JIS
  | Cld2Encoding_JAPANESE_JIS
  | Cld2Encoding_CHINESE_BIG5
  | Cld2Encoding_CHINESE_GB
  | Cld2Encoding_CHINESE_EUC_CN
  | Cld2Encoding_KOREAN_EUC_KR
  | Cld2Encoding_UNICODE_UNUSED
  | Cld2Encoding_CHINESE_EUC_DEC
  | Cld2Encoding_CHINESE_CNS
  | Cld2Encoding_CHINESE_BIG5_CP950
  | Cld2Encoding_JAPANESE_CP932
  | Cld2Encoding_UTF8
  | Cld2Encoding_UNKNOWN_ENCODING
  | Cld2Encoding_ASCII_7BIT
  | Cld2Encoding_RUSSIAN_KOI8_R
  | Cld2Encoding_RUSSIAN_CP1251
  | Cld2Encoding_MSFT_CP1252
  | Cld2Encoding_RUSSIAN_KOI8_RU
  | Cld2Encoding_MSFT_CP1250
  | Cld2Encoding_ISO_8859_15
  | Cld2Encoding_MSFT_CP1254
  | Cld2Encoding_MSFT_CP1257
  | Cld2Encoding_ISO_8859_11
  | Cld2Encoding_MSFT_CP874
  | Cld2Encoding_MSFT_CP1256
  | Cld2Encoding_MSFT_CP1255
  | Cld2Encoding_ISO_8859_8_I
  | Cld2Encoding_HEBREW_VISUAL
  | Cld2Encoding_CZECH_CP852
  | Cld2Encoding_CZECH_CSN_369103
  | Cld2Encoding_MSFT_CP1253
  | Cld2Encoding_RUSSIAN_CP866
  | Cld2Encoding_ISO_8859_13
  | Cld2Encoding_ISO_2022_KR
  | Cld2Encoding_GBK
  | Cld2Encoding_GB18030
  | Cld2Encoding_BIG5_HKSCS
  | Cld2Encoding_ISO_2022_CN
  | Cld2Encoding_TSCII
  | Cld2Encoding_TAMIL_MONO
  | Cld2Encoding_TAMIL_BI
  | Cld2Encoding_JAGRAN
  | Cld2Encoding_MACINTOSH_ROMAN
  | Cld2Encoding_UTF7
  | Cld2Encoding_BHASKAR
  | Cld2Encoding_HTCHANAKYA
  | Cld2Encoding_UTF16BE
  | Cld2Encoding_UTF16LE
  | Cld2Encoding_UTF32BE
  | Cld2Encoding_UTF32LE
  | Cld2Encoding_BINARYENC
  | Cld2Encoding_HZ_GB_2312
  | Cld2Encoding_UTF8UTF8
  | Cld2Encoding_TAM_ELANGO
  | Cld2Encoding_TAM_LTTMBARANI
  | Cld2Encoding_TAM_SHREE
  | Cld2Encoding_TAM_TBOOMIS
  | Cld2Encoding_TAM_TMNEWS
  | Cld2Encoding_TAM_WEBTAMIL
  | Cld2Encoding_KDDI_SHIFT_JIS
  | Cld2Encoding_DOCOMO_SHIFT_JIS
  | Cld2Encoding_SOFTBANK_SHIFT_JIS
  | Cld2Encoding_KDDI_ISO_2022_JP
  | Cld2Encoding_SOFTBANK_ISO_2022_JP
  deriving (Eq,Ord,Show,Bounded,Enum,Typeable,Data,Generic,Hashable)

-- | A collection of contextual clues which can help improve the
-- accuracy of language detection
data Hints =
    Hints { -- | The value of the @Content-Language@ HTTP header
            hintContentLanguage :: Maybe String,
            -- | The TLD of the website which served the corpus being analyzed
            hintTLD :: Maybe String,
            -- | The original character encoding of the corpus
            hintEncoding :: Encoding,
            -- | A hint from any other available context
            hintLanguage :: Language }
             deriving (Eq,Ord,Show,Typeable,Data)

-- | The default set of hints, which is @Hints@ @Nothing@ @Nothing@ @Cld2Encoding_UNKNOWN_ENCODING@ @Cld2Language_UNKNOWN_LANGUAGE@
defaultHints :: Hints
defaultHints = Hints Nothing Nothing Cld2Encoding_UNKNOWN_ENCODING Cld2Language_UNKNOWN_LANGUAGE

-- | Flags which cause CLD2 to dump debugging output to stderr.
data DebugFlags = DebugFlags { debugFlagScoreAsQuads :: Bool,
                               debugFlagHtml :: Bool,
                               debugFlagCr :: Bool,
                               debugFlagVerbose :: Bool,
                               debugFlagQuiet :: Bool,
                               debugFlagEcho :: Bool }
                  deriving (Eq,Ord,Show,Typeable,Data)

-- | The default set of debugging flags, all @False@
defaultDebugFlags :: DebugFlags
defaultDebugFlags = DebugFlags False False False False False False

-- | Represents a range of text and its detected language
data Chunk =
    Chunk { -- | The offset of the start of the chunk, in bytes
            chunkOffset :: Int,
            -- | The size of the chunk, in bytes
            chunkSize :: Int,
            -- | The detected language of this chunk
            chunkLanguage :: Language }
             deriving (Eq,Ord,Show,Typeable,Data)

-- | The result of performing language detection on a corpus
data Result = Result {
      -- | The primary language of the corpus
      resultSimple :: Language,
      -- | The top three most prevalent languages in the corpus
      resultTop3 :: (Language, Language, Language),
      -- | The
      resultTop3Percent :: (Int, Int, Int),
      -- | Confidence scores for the top three most prevalent languages
      resultTop3Score :: (Double, Double, Double),
      -- | Identifies the language of each chunk of the corpus
      resultChunks :: [Chunk],
      -- | The size of the corpus that was analyzed
      resultTextBytes :: Int,
      -- | Whether this result should be considered reliable
      resultIsReliable :: Bool }
              deriving (Eq,Ord,Show,Typeable,Data)

foreign import ccall "cld2_haskell_shim" c_cld2_haskell_shim ::
  Ptr CInt -> Ptr CChar -> CInt -> CInt -> CString -> CString -> CInt -> CInt
  -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CSize
  -> Ptr (Ptr CInt) -> Ptr (Ptr CShort) -> Ptr (Ptr CShort) -> Ptr CInt
  -> Ptr CInt -> IO CInt

boolToCInt :: Bool -> CInt
boolToCInt False = 0
boolToCInt True = 1

cIntToBool :: CInt -> Bool
cIntToBool 0 = False
cIntToBool _ = True

takeBit :: (Bits a) => a -> Bool -> a
takeBit x True = x
takeBit x False = zeroBits

flagsToCInt :: DebugFlags -> CInt
flagsToCInt (DebugFlags a b c d e f) =
    takeBit 0x0100 a .|.
    takeBit 0x0200 b .|.
    takeBit 0x0400 c .|.
    takeBit 0x0800 d .|.
    takeBit 0x1000 e .|.
    takeBit 0x2000 f

withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString (Just str) f = withCString str f
withMaybeCString Nothing f = f nullPtr

-- | This function is the most general way to invoke CLD2. Since setting
-- debug flags can cause output on stderr, the result is returned in the IO
-- monad.
detectLanguageDebug ::
    Text -- ^ The corpus to be analyzed
    -> Bool -- ^ True for plain text, False for HTML
    -> Hints
    -> DebugFlags
    -> IO Result
detectLanguageDebug text isPlainText hints flags =
  unsafeUseAsCStringLen (encodeUtf8 text) $ \(cStr,cLen) ->
    withMaybeCString (hintContentLanguage hints) $ \cContentLanguage ->
      withMaybeCString (hintTLD hints) $ \cTld ->
        allocaArray 3 $ \cLanguage3 ->
          allocaArray 3 $ \cPercent3 ->
            allocaArray 3 $ \cScore3 ->
              alloca $ \cNumChunksPtr ->
                alloca $ \cChunkOffsetsPtr ->
                  alloca $ \cChunkSizesPtr ->
                    alloca $ \cChunkLangsPtr ->
                      alloca $ \cTextBytesPtr ->
                        alloca $ \cIsReliablePtr ->
                          alloca $ \cResultLangPtr -> do
                            let cIsPlainText = boolToCInt isPlainText
                            let cEncodingHint =
                                    toEnum . fromEnum $ hintEncoding hints
                            let cLanguageHint =
                                    toEnum . fromEnum $ hintLanguage hints
                            let cFlags = flagsToCInt flags
                            let cBufferLen = toEnum cLen
                            (cChunkOffsets,cChunkSizes,cChunkLangs) <-
                                mask_ $ do
                                  cResult <-
                                      c_cld2_haskell_shim
                                      cResultLangPtr cStr cBufferLen
                                      cIsPlainText cContentLanguage cTld
                                      cEncodingHint cLanguageHint cFlags
                                      cLanguage3 cPercent3 cScore3
                                      cNumChunksPtr cChunkOffsetsPtr
                                      cChunkSizesPtr cChunkLangsPtr
                                      cTextBytesPtr cIsReliablePtr
                                  if (Errno cResult) == eNOMEM then
                                    throwIO HeapOverflow
                                  else if (Errno cResult) == eOK then
                                    do cNumChunks <-
                                           fromEnum <$> peek cNumChunksPtr
                                       cChunkOffsetsArray <-
                                           peek cChunkOffsetsPtr
                                       cChunkSizesArray <- peek cChunkSizesPtr
                                       cChunkLangsArray <- peek cChunkLangsPtr
                                       cChunkOffsets <-
                                          peekArray cNumChunks
                                                    cChunkOffsetsArray
                                       cChunkSizes <-
                                           peekArray cNumChunks cChunkSizesArray
                                       cChunkLangs <-
                                           peekArray cNumChunks cChunkLangsArray
                                       free cChunkOffsetsArray
                                       free cChunkSizesArray
                                       free cChunkLangsArray
                                       return (cChunkOffsets, cChunkSizes,
                                               cChunkLangs)
                                  else
                                    throwIO $
                                    AssertionFailed "unknown error in CLD2"
                            cResultLang <- peek cResultLangPtr
                            [cLanguage0,cLanguage1,cLanguage2] <-
                                peekArray 3 cLanguage3
                            [cPercent0, cPercent1, cPercent2] <-
                                peekArray 3 cPercent3
                            [cScore0, cScore1, cScore2] <-
                                peekArray 3 cScore3
                            cNumChunks <- fromEnum <$> peek cNumChunksPtr
                            cTextBytes <- peek cTextBytesPtr
                            cIsReliable <- peek cIsReliablePtr
                            let theSimple =
                                    toEnum . fromEnum $ cResultLang
                            let theTop3 =
                                    (toEnum . fromEnum $ cLanguage0,
                                     toEnum . fromEnum $ cLanguage1,
                                     toEnum . fromEnum $ cLanguage2)
                            let theTop3Percent =
                                    (fromEnum cPercent0,
                                     fromEnum cPercent1,
                                     fromEnum cPercent2)
                            let (CDouble theScore0) = cScore0
                            let (CDouble theScore1) = cScore1
                            let (CDouble theScore2) = cScore2
                            let theChunks =
                                    (flip map)
                                    (zip3 cChunkOffsets cChunkSizes
                                          cChunkLangs)
                                    (\(offset,size,language) ->
                                         Chunk
                                         (fromEnum offset)
                                         (fromEnum size)
                                         (toEnum . fromEnum $ language))
                            let theTextBytes = fromEnum cTextBytes
                            let theIsReliable = cIntToBool cIsReliable
                            return $ Result
                                   theSimple theTop3 theTop3Percent
                                   (theScore0, theScore1, theScore2)
                                   theChunks theTextBytes theIsReliable

-- | Call 'detectLanguageDebug' with all debug flags disabled and
-- call 'unsafePerformIO' on the result. This is the recommended
-- function for most use cases.
detectLanguage ::
    Text  -- ^ The corpus to be analyzed
    -> Bool -- ^ True for plain text, False for HTML
    -> Hints -> Result
detectLanguage text isPlainText hints =
    unsafePerformIO $
    detectLanguageDebug text isPlainText hints defaultDebugFlags

-- | Call 'detectLanguage' on HTML input with no hints and return the
-- @resultSimple@ field of the result.
detectLanguageSimple :: Text -> Language
detectLanguageSimple text =
    resultSimple $ detectLanguage text False defaultHints
