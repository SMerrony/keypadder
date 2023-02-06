--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Containers.Indefinite_Ordered_Maps;
with Interfaces; use Interfaces;

package Keys is

   package Key_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type => String, Element_Type => Unsigned_16);
   use Key_Maps;

   Keys_M : Map;

   procedure Setup_Key_Map;

   procedure List_All_Keys;

end Keys;