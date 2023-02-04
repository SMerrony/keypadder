--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

package body Uinput is

   procedure Emit (FID : K_File_ID_T;
                   E_Type, E_Code : K_U16_T;
                   E_Val : K_Int_T) is
      IE         : K_Input_Event_T;
      Dummy_SS   : K_SSize_T;
      IE_Address : constant System.Address := IE'Address;
   begin
      IE.IE_Type  := E_Type;
      IE.IE_Code  := E_Code;
      IE.IE_Value := E_Val;

      K_Write_IE
        (Written => Dummy_SS,
         FID     => FID,
         IE_Addr => IE_Pointers.To_Pointer (IE_Address),
         Count   => K_Input_Event_Size);
   end Emit;

end Uinput;