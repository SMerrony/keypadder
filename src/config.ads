--  SPDX-License-Identifier: GPL-3.0-or-later

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package Config is

   type Port_T is range 1 .. 65535;
   type Keypadder_Conf_T is record
      Port : Port_T;
   end record;

   Keypadder_Conf : Keypadder_Conf_T;

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
   Tabs_Count : Natural := 0;

   type Tabs_T is array (1 .. Max_Tabs) of Tab_T;

   Tabs : Tabs_T;

   procedure Load_Config_File (Filename : String; Verbose : Boolean := False);

   Could_Not_Parse,
   Duplicate_Configuration,
   Incomplete_Configuration,
   Unknown_Configuration_Item  : exception;

end Config;
