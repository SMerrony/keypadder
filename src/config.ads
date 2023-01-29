--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2023 Stephen Merrony

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package Config is

   type Port_T is range 1 .. 65535;
   type Keypadder_Conf_T is record
      Port : Port_T;
   end record;

   type Key_T is record
      Label   : Unbounded_String;
      Send    : Unbounded_String;
      Colspan,
      Rowspan : Natural := 0;
   end record;

   Max_Keys : constant Positive := 88;
   --  Arbitrary limit to ensure the config parser doesn't go into
   --  a crazy loop.

   type Keys_T is array (1 .. Max_Keys) of Key_T;

   type Tab_T is record
      Label      : Unbounded_String;
      Columns    : Natural := 0;
      Keys       : Keys_T;
      Keys_Count : Natural := 0;
   end record;

   Max_Tabs   : constant Positive := 8;
   type Tabs_T is array (1 .. Max_Tabs) of Tab_T;

   type Conf_T is record
      Keypadder_Conf : Keypadder_Conf_T;
      Tabs_Count     : Natural := 0;
      Tabs           : Tabs_T;
   end record;

   Conf : Conf_T;

   function Load_Config_File (Filename : String; Verbose : Boolean := False) return Boolean;

   Could_Not_Parse,
   Duplicate_Configuration,
   Incomplete_Configuration,
   Unknown_Configuration_Item  : exception;

end Config;
