--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Interfaces; use Interfaces;

package Keys is

   package Key_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Unsigned_16,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");
   use Key_Maps;

   Keys_M : Map;

   procedure Setup_Key_Map;

end Keys;