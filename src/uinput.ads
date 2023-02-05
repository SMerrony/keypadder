--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

--  Uinput provides the 64-bit Linux low-level I/O functions we need.

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System;
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
   type K_Ioctl_ID_T is new Unsigned_32;
   type K_Dummy_T    is new Integer_32;

   UINPUT_IOCTL_BASE : constant Integer_32 := 85; --  ASCII 'U'

   Uinput_Max_Name_Size : constant Integer := 80;
   BUS_USB : constant K_U16_T := 3;

   type K_Input_ID_T is record
      K_Bustype,
      K_Vendor,
      K_Product,
      K_Version : K_U16_T;
   end record;
   pragma Convention (C, K_Input_ID_T);

   type K_Uinput_Setup_T is record
      ID             : K_Input_ID_T;
      Name           : String (1 .. Uinput_Max_Name_Size);
      FF_Effects_Max : K_U32_T := 0;
   end record;
   pragma Convention (C, K_Uinput_Setup_T);

   UI_SET_EVBIT  : constant Unsigned_32 := 100;
   UI_SET_KEYBIT : constant Unsigned_32 := 101;

   UI_DEV_SETUP   : constant Unsigned_32 := 3;
   UI_DEV_CREATE  : constant Unsigned_32 := 1;
   UI_DEV_DESTROY : constant Unsigned_32 := 2;

   type K_Timeval_T is record
      TV_Sec  : K_Int_T  := 0;
      TV_USec : K_Long_T := 0;
   end record;

   --  The Event Types we need...
   EV_SYN : constant K_U16_T := 0;
   EV_KEY : constant K_U16_T := 1;
   EV_REL : constant K_U16_T := 2;

   type K_Input_Event_T is record
      Time     : K_Timeval_T;
      IE_Type  : K_U16_T;
      IE_Code  : K_U16_T;
      IE_Value : K_Int_T;
   end record;
   pragma Convention (C, K_Input_Event_T);

   K_Input_Event_Size : constant K_Size_T := K_Input_Event_T'Size / 8;

   package IE_Pointers is new System.Address_To_Access_Conversions (K_Input_Event_T);

   --  The short-form 2 arg (i.e. 1 parameter) ioctl call
   function K_IOCTL_2_Arg (FID      : K_File_ID_T;
                           IOCTL_ID : K_Ioctl_ID_T;
                           Ignored  : K_Dummy_T := 0
                          ) return K_Int_T;
   pragma Import (C, K_IOCTL_2_Arg, "ioctl");

   --  The long-form 3 arg (i.e. 2 parameters) ioctl call
   --  with a simple (int) 3rd argument
   function K_IOCTL_3_Arg (FID       : K_File_ID_T;
                           IOCTL_ID  : K_Ioctl_ID_T;
                           IOCTL_Arg : K_Long_T
                          ) return K_Int_T;
   pragma Import (C, K_IOCTL_3_Arg, "ioctl");

   --  The long-form 3 arg (i.e. 2 parameters) ioctl call
   --  with an (address of) Uinput_Setup 3rd argument
   function K_IOCTL_Uinput_Setup (FID               : K_File_ID_T;
                                  IOCTL_ID          : K_Ioctl_ID_T;
                                  Usetup            : K_Uinput_Setup_T
                                 ) return K_Int_T;
   pragma Import (C, K_IOCTL_Uinput_Setup, "ioctl");

   procedure C_Perror (Msg : String);
   pragma Import (C, C_Perror, "perror");

   O_WRONLY   : constant K_Int_T := 1;
   O_NONBLOCK : constant K_Int_T := 2048;

   function K_Open (Path  : String;
                    Flags : K_Int_T) return K_File_ID_T;
   pragma Import (C, K_Open, "open");

   --  System file write call
   function K_Write_IE (FID     : K_File_ID_T;
                        IE      : K_Input_Event_T;
                        Count   : K_Size_T
                       ) return K_SSize_T;
   pragma Import (C, K_Write_IE, "write");

   --  System file close call
   function K_Close (FID : K_File_ID_T) return K_Int_T;
   pragma Import (C, K_Close, "close");

   --  The special Event Code we need...
   SYN_REPORT : constant K_U16_T := 0;

   pragma Warnings (Off, "types for unchecked conversion have different sizes");
   function Char_To_K_U16 is new Ada.Unchecked_Conversion (Character, K_U16_T);
   pragma Warnings (On, "types for unchecked conversion have different sizes");

   Cannot_Open,
   Cannot_Write,
   IOCTL_Error   : exception;

   --  IOW is analagous to the _IOW macro in include/uapi/asm-generic/ioctl.h
   function IOW (Code : Unsigned_32; Len : Integer) return K_Ioctl_ID_T;

   --  IO is analagous to the _IO macro in include/uapi/asm-generic/ioctl.h
   function IO (Code : Unsigned_32; Len : Integer) return K_Ioctl_ID_T;

   procedure Emit (FID            : K_File_ID_T;
                   E_Type, E_Code : K_U16_T;
                   E_Val          : K_Int_T);

end Uinput;