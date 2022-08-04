{*
 * << Haru Free PDF Library 2.0.3 >> -- hpdf.pas
 *
 * Copyright (c) 1999-2006 Takeshi Kanno <takeshi_kanno@est.hi-ho.ne.jp>
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.
 * It is provided "as is" without express or implied warranty.
 *
 *}

unit hpdf_consts;

interface

uses
  hpdf_types;

const
  HPDF_TRUE = 1;
  HPDF_FALSE = 0;

  HPDF_OK = 0;
  HPDF_NOERROR = 0;

{*----- default values -------------------------------------------------------*}

{* buffer size which is required when we convert to character string. *}
  HPDF_TMP_BUF_SIZ = 256;
  HPDF_SHORT_BUF_SIZ = 32;
  HPDF_REAL_LEN = 11;
  HPDF_INT_LEN = 11;
  HPDF_TEXT_DEFAULT_LEN = 256;
  HPDF_UNICODE_HEADER_LEN = 2;
  HPDF_DATE_TIME_STR_LEN = 23;

{* length of each item defined in PDF *}
  HPDF_BYTE_OFFSET_LEN = 10;
  HPDF_OBJ_ID_LEN = 7;
  HPDF_GEN_NO_LEN = 5;

{* default value of Graphic State *}
  HPDF_DEF_FONT: string = 'Helvetica';
  HPDF_DEF_PAGE_LAYOUT = HPDF_PAGE_LAYOUT_SINGLE;
  HPDF_DEF_PAGE_MODE = HPDF_PAGE_MODE_USE_NONE;
  HPDF_DEF_WORDSPACE= 0;
  HPDF_DEF_CHARSPACE= 0;
  HPDF_DEF_FONTSIZE = 10;
  HPDF_DEF_HSCALING = 100;
  HPDF_DEF_LEADING= 0;
  HPDF_DEF_RENDERING_MODE = HPDF_FILL;
  HPDF_DEF_RAISE= 0;
  HPDF_DEF_LINEWIDTH = 1;
  HPDF_DEF_LINECAP =  HPDF_BUTT_END;
  HPDF_DEF_LINEJOIN = HPDF_MITER_JOIN;
  HPDF_DEF_MITERLIMIT = 10;
  HPDF_DEF_FLATNESS = 1;
  HPDF_DEF_PAGE_NUM = 1;

  HPDF_BS_DEF_WIDTH = 1;

{* defalt page-size *}
  HPDF_DEF_PAGE_WIDTH = 595.276;
  HPDF_DEF_PAGE_HEIGHT = 841.89;


{*---------------------------------------------------------------------------*}
{*----- compression mode ----------------------------------------------------*}

  HPDF_COMP_NONE = $00;
  HPDF_COMP_TEXT = $01;
  HPDF_COMP_IMAGE = $02;
  HPDF_COMP_METADATA = $04;
  HPDF_COMP_ALL = $0F;
{*  HPDF_COMP_BEST_COMPRESS = $10;
 *  HPDF_COMP_BEST_SPEED = $20;
 *}
  HPDF_COMP_MASK = $FF;

{*----------------------------------------------------------------------------*}
{*----- permission flags (only Revision 2 is supported)-----------------------*}

  HPDF_ENABLE_READ = 0;
  HPDF_ENABLE_PRINT = 4;
  HPDF_ENABLE_EDIT_ALL = 8;
  HPDF_ENABLE_COPY = 16;
  HPDF_ENABLE_EDIT = 32;


{*----------------------------------------------------------------------------*}
{*------ viewer preferences definitions --------------------------------------*}

  HPDF_HIDE_TOOLBAR = 1;
  HPDF_HIDE_MENUBAR = 2;
  HPDF_HIDE_WINDOW_UI = 4;
  HPDF_FIT_WINDOW = 8;
  HPDF_CENTER_WINDOW = 16;


{*---------------------------------------------------------------------------*}
{*------ limitation of object implementation (PDF1.4) -----------------------*}

  HPDF_LIMIT_MAX_INT = 2147483647;
  HPDF_LIMIT_MIN_INT = -2147483647;

  HPDF_LIMIT_MAX_REAL = 32767;
  HPDF_LIMIT_MIN_REAL = -32767;

  HPDF_LIMIT_MAX_STRING_LEN = 65535;
  HPDF_LIMIT_MAX_NAME_LEN = 127;

  HPDF_LIMIT_MAX_ARRAY = 8191;
  HPDF_LIMIT_MAX_DICT_ELEMENT = 4095;
  HPDF_LIMIT_MAX_XREF_ELEMENT = 8388607;
  HPDF_LIMIT_MAX_GSTATE = 28;
  HPDF_LIMIT_MAX_DEVICE_N = 8;
  HPDF_LIMIT_MAX_DEVICE_N_V15 = 32;
  HPDF_LIMIT_MAX_CID = 65535;
  HPDF_MAX_GENERATION_NUM = 65535;

  HPDF_MIN_PAGE_HEIGHT = 3;
  HPDF_MIN_PAGE_WIDTH = 3;
  HPDF_MAX_PAGE_HEIGHT = 14400;
  HPDF_MAX_PAGE_WIDTH = 14400;
  HPDF_MIN_MAGNIFICATION_FACTOR = 8;
  HPDF_MAX_MAGNIFICATION_FACTOR = 3200;

{*---------------------------------------------------------------------------*}
{*------ limitation of various properties -----------------------------------*}

  HPDF_MIN_PAGE_SIZE = 3;
  HPDF_MAX_PAGE_SIZE = 14400;
  HPDF_MIN_HORIZONTALSCALING = 10;
  HPDF_MAX_HORIZONTALSCALING = 300;
  HPDF_MIN_WORDSPACE = -30;
  HPDF_MAX_WORDSPACE = 300;
  HPDF_MIN_CHARSPACE = -30;
  HPDF_MAX_CHARSPACE = 300;
  HPDF_MAX_FONTSIZE = 300;
  HPDF_MAX_ZOOMSIZE = 10;
  HPDF_MAX_LEADING = 300;
  HPDF_MAX_LINEWIDTH = 100;
  HPDF_MAX_DASH_PATTERN = 100;

{*----------------------------------------------------------------------------*}
{*----- country code definition ----------------------------------------------*}

  //removed to reduce code size

{*----------------------------------------------------------------------------*}
{*----- Graphis mode ---------------------------------------------------------*}

  HPDF_GMODE_PAGE_DESCRIPTION = $0001;
  HPDF_GMODE_PATH_OBJECT = $0002;
  HPDF_GMODE_TEXT_OBJECT = $0004;
  HPDF_GMODE_CLIPPING_PATH = $0008;
  HPDF_GMODE_SHADING = $0010;
  HPDF_GMODE_INLINE_IMAGE =  $0020;
  HPDF_GMODE_EXTERNAL_OBJECT = $0040;

{*----------------------------------------------------------------------------*}

implementation

end.

