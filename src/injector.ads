--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Unchecked_Conversion;

with Interfaces; use Interfaces;

package Injector is

   Linux_Path : constant String := "/dev/uinput";

   type Timeval_T is record
      TV_Sec  : Integer_32 := 0;
      TV_USec : Integer_64 := 0;
   end record;

   --  The Event Types we need...
   EV_SYN : constant Unsigned_16 := 0;
   EV_KEY : constant Unsigned_16 := 1;
   EV_REL : constant Unsigned_16 := 2;

   --  The special Event Code we need...
   SYN_REPORT : constant Unsigned_16 := 0;

   type Input_Event_T is record
      Time     : Timeval_T;
      IE_Type  : Unsigned_16;
      IE_Code  : Unsigned_16;
      IE_Value : Integer_32;
   end record;

   function Char_To_U16 is new Ada.Unchecked_Conversion (Character, Unsigned_16);

   task Injector_Task is
      entry Start;
      entry Shutdown;
      entry Send (Keys : String);
   end Injector_Task;

   Cannot_Open, Cannot_Write : exception;

end Injector;