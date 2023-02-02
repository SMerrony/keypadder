--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System.Address_To_Access_Conversions;

package Uinput is

   Linux_Path : constant String := "/dev/uinput";

   --  fundamental kernel types
   type K_U16_T      is new Unsigned_16;
   type K_S32_T      is new Integer_32;
   type K_U32_T      is new Unsigned_32;
   type K_Int_T      is new Integer_32;
   type K_Long_T     is new Unsigned_64; --  assuming 64-bit arch
   type K_Size_T     is new Unsigned_64;
   type K_SSize_T    is new Integer_64;
   type K_File_ID_T  is new Integer_32;
   type K_Ioctl_ID_T is new Integer_32;
   type K_Dummy_T    is new Integer_32;

   Uinput_Max_Name_Size : constant Integer := 80;

   type K_Input_ID_T is record
      K_Bustype,
      K_Vendor,
      K_Product,
      K_Version : K_U16_T;
   end record;

   type K_Uinput_Setup_T is record
      ID             : K_Input_ID_T;
      Name           : String (1 .. Uinput_Max_Name_Size);
      FF_Effects_Max : K_U32_T;
   end record;

   package Uinput_Setup_Pointers is new System.Address_To_Access_Conversions (K_Uinput_Setup_T);

   type K_Timeval_T is record
      TV_Sec  : K_Int_T  := 0;
      TV_USec : K_Long_T := 0;
   end record;

   --  The Event Types we need...
   EV_SYN : constant K_Int_T := 0;
   EV_KEY : constant K_Int_T := 1;
   EV_REL : constant K_Int_T := 2;

   type K_Input_Event_T is record
      Time     : K_Timeval_T;
      IE_Type  : K_Int_T;
      IE_Code  : K_Int_T;
      IE_Value : K_Int_T;
   end record;

   K_Input_Event_Size : constant K_Size_T := K_Input_Event_T'Size;

   package IE_Pointers is new System.Address_To_Access_Conversions (K_Input_Event_T);

   --  The short-form 2 arg (i.e. 1 parameter) ioctl call
   procedure K_IOCTL_2_Arg (Result   : out K_Int_T;
                            FID      : K_File_ID_T;
                            IOCTL_ID : K_Ioctl_ID_T;
                            Ignored  : K_Dummy_T);
   pragma Import (C, K_IOCTL_2_Arg, "ioctl");
   pragma Import_Valued_Procedure (K_IOCTL_2_Arg);

   --  The long-form 3 arg (i.e. 2 parameters) ioctl call
   --  with a simple (int) 3rd argument
   procedure K_IOCTL_3_Arg (Result    : out K_Int_T;
                            FID       : K_File_ID_T;
                            IOCTL_ID  : K_Ioctl_ID_T;
                            IOCTL_Arg : K_Int_T);
   pragma Import (C, K_IOCTL_3_Arg, "ioctl");
   pragma Import_Valued_Procedure (K_IOCTL_3_Arg);

   --  The long-form 3 arg (i.e. 2 parameters) ioctl call
   --  with an (address of) Uinput_Setup 3rd argument
   procedure K_IOCTL_Uinput_Setup (Result            : out K_Int_T;
                                   FID               : K_File_ID_T;
                                   IOCTL_ID          : K_Ioctl_ID_T;
                                   Uinput_Setup_Addr : Uinput_Setup_Pointers.Object_Pointer);
   pragma Import (C, K_IOCTL_Uinput_Setup, "ioctl");
   pragma Import_Valued_Procedure (K_IOCTL_Uinput_Setup);

   O_WRONLY   : constant K_Int_T := 1;
   O_NONBLOCK : constant K_Int_T := 2048;

   --  System file open call
   procedure K_Open (FID  : out K_File_ID_T;
                     Path : String;
                     Flags : K_Int_T);
   pragma Import (C, K_Open, "open");
   pragma Import_Valued_Procedure (K_Open);

   --  System file write call
   procedure K_Write_IE (Written : out K_SSize_T;
                      FID     : K_File_ID_T;
                      IE_Addr : IE_Pointers.Object_Pointer;
                      Count   : K_Size_T);
   pragma Import (C, K_Write_IE, "write");
   pragma Import_Valued_Procedure (K_Write_IE);

   --  System file close call
   procedure K_Close (Result : out K_Int_T;
                      FID    : K_File_ID_T);
   pragma Import (C, K_Close, "close");
   pragma Import_Valued_Procedure (K_Close);

   --  The special Event Code we need...
   SYN_REPORT : constant K_Int_T := 0;

   function Char_To_K_Int is new Ada.Unchecked_Conversion (Character, K_Int_T);

   Cannot_Open, Cannot_Write : exception;

   procedure Emit (FID : K_File_ID_T; E_Type, E_Code, E_Val : K_Int_T);

end Uinput;