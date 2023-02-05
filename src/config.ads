--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2023 Stephen Merrony

with Ada.Containers;            use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with Interfaces; use Interfaces;

package Config is

   type Port_T is range 1 .. 65535;
   type Keypadder_Conf_T is record
      Port : Port_T;
   end record;

   type Up_Down_T is (Up, Down);
   type Send_Event_T is record
   --  Our internal representation of what needs to be sent
   --  as a USB event.
      Up_Down : Up_Down_T;
      Value   : Unsigned_16;
   end record;

   package Event_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Send_Event_T);
   use Event_Vectors;

   type Key_T is record
      Label       : Unbounded_String;
      Send        : Unbounded_String;  -- the undecoded string from the TOML
      Send_Events : Vector;
      Colspan,
      Rowspan     : Natural := 0;
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
   Unknown_Configuration_Item,
   Unknown_Key                 : exception;

end Config;
