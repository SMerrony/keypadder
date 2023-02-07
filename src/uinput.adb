--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

package body Uinput is

   --  IOW is analagous to the _IOW macro in include/uapi/asm-generic/ioctl.h
   function IOW (Code : Unsigned_32; Len : Integer) return K_Ioctl_ID_T is
      Tmp : Unsigned_32 := Code;
   begin
      Tmp := Tmp + Shift_Left (Unsigned_32 (UINPUT_IOCTL_BASE), 8);
      Tmp := Tmp + Shift_Left (Unsigned_32 (Len), 16);  --  arg length
      Tmp := Tmp + Shift_Left (Unsigned_32 (1), 30);  --  direction (write, out)
      return K_Ioctl_ID_T (Tmp);
   end IOW;

   --  IO is analagous to the _IO macro in include/uapi/asm-generic/ioctl.h
   function IO (Code : Unsigned_32; Len : Integer) return K_Ioctl_ID_T is
      Tmp : Unsigned_32 := Code;
   begin
      Tmp := Tmp + Shift_Left (Unsigned_32 (UINPUT_IOCTL_BASE), 8);
      Tmp := Tmp + Shift_Left (Unsigned_32 (Len), 16);  --  arg length
      return K_Ioctl_ID_T (Tmp);
   end IO;

   procedure Emit (FID : K_File_ID_T;
                   E_Type, E_Code : K_U16_T;
                   E_Val : K_Int_T) is
      IE         : K_Input_Event_T;
      Write_SS   : K_SSize_T;
   begin
      IE.IE_Type  := E_Type;
      IE.IE_Code  := E_Code;
      IE.IE_Value := E_Val;

      Write_SS := K_Write_IE (FID, IE, K_Input_Event_Size);
      if Write_SS = -1 then
         C_Perror ("Write failed...");
         raise Cannot_Write with "Write failed";
      end if;
   end Emit;

end Uinput;