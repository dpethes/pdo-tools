unit dc2_common; //r204
{$mode objfpc}{$H+}

interface

const
  MAX_BLOCK_SIZE = 32 * 1024;

type
  TBlockTypeEnum = (BTRaw = 0, BTFixed, BTDynamic, BTError);

  //encoding statistics
  TStats = record
      elements_encoded: int64;
      blocks: array[TBlockTypeEnum] of longword;
  end;

implementation

end.

(*******************************************************************************
common.pas
Copyright (c) 2009-2018 David Pethes

This file is part of Dc2.

Dc2 is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Dc2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Dc2.  If not, see <http://www.gnu.org/licenses/>.

*******************************************************************************)
