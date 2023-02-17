--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2023 Stephen Merrony

with Ada.Containers;            use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with Interfaces; use Interfaces;

with TOML;

package Config is

   Default_Max_Tabbed : constant Positive := 7;
   --  Unless otherwise specified in the configuration, this is the
   --  maximum number of tabs before we switch to a dropdown selector.

   type Port_T      is range 1 .. 65535;
   type Tabswitch_T is (Unset, Dropdown, Tabs);
   type Keypadder_Conf_T is record
      Port      : Port_T;
      Tabswitch : Tabswitch_T := Unset;
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

   type Key_T is record
      Label       : Unbounded_String;
      Send        : Unbounded_String;  -- the undecoded string from the TOML
      Send_Events : Event_Vectors.Vector;
      Colspan,
      Rowspan     : Natural := 0;
   end record;

   package Key_Vectors is new
      Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Key_T);

   type Tab_T is record
      Label      : Unbounded_String;
      Columns    : Natural := 0;
      Keys       : Key_Vectors.Vector;
   end record;

   Max_Tabs   : constant Positive := 32;
   --  Absolute maximum number of tabs.
   type Tabs_T is array (1 .. Max_Tabs) of Tab_T;

   type Conf_T is record
      Keypadder_Conf : Keypadder_Conf_T;
      Tabs_Count     : Natural := 0;
      Tabs           : Tabs_T;
   end record;

   Conf : Conf_T;

   function Parse_And_Check_Config (Filename : String) return TOML.Read_Result
      with Pre => (Filename'Length > 0);

   function Load_Config_File (Filename : String; Verbose : Boolean := False) return Boolean
      with Pre => (Filename'Length > 0);

   Could_Not_Parse,
   Duplicate_Configuration,
   Incomplete_Configuration,
   Invalid_Value,
   Too_Many_Keys, Too_Many_Tabs,
   Unknown_Configuration_Item,
   Unknown_Key                 : exception;

end Config;
