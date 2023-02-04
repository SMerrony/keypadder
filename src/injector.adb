--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Text_IO;
with Interfaces;  use Interfaces;

with Uinput;      use Uinput;

package body Injector is

   task body Injector_Task is
      Uinput_FID  : K_File_ID_T;
      Usetup      : K_Uinput_Setup_T;
      IE_Time     : K_Timeval_T;  --  defaults to zeroes
      Key_IE      : K_Input_Event_T;
      Report_IE   : constant K_Input_Event_T := (Time     => IE_Time,
                                                 IE_Type  => EV_SYN,
                                                 IE_Code  => SYN_REPORT,
                                                 IE_Value => 0);
      Result      : K_Int_T;
      Key_Val     : K_Int_T := 0;
      Request     : K_Ioctl_ID_T;

      function IOW (Code : Unsigned_32; Len : Integer) return K_Ioctl_ID_T is
         Tmp : Unsigned_32 := Code;
      begin
         Tmp := Tmp + Shift_Left (Unsigned_32 (UINPUT_IOCTL_BASE), 8);
         Tmp := Tmp + Shift_Left (Unsigned_32 (Len), 16);  --  arg length of 4 (?)
         Tmp := Tmp + Shift_Left (Unsigned_32 (1), 30);  --  direction (write, out)
         return K_Ioctl_ID_T (Tmp);
      end IOW;
      function IO (Code : Unsigned_32; Len : Integer) return K_Ioctl_ID_T is
         Tmp : Unsigned_32 := Code;
      begin
         Tmp := Tmp + Shift_Left (Unsigned_32 (UINPUT_IOCTL_BASE), 8);
         Tmp := Tmp + Shift_Left (Unsigned_32 (Len), 16);  --  arg length of 4 (?)
         return K_Ioctl_ID_T (Tmp);
      end IO;
   begin
      accept Start do
         Uinput_FID := K_Open (Linux_Path, O_WRONLY + O_NONBLOCK);
         if Uinput_FID = -1 then
            Ada.Text_IO.Put_Line ("ERROR: Could not open " & Linux_Path);
            raise Cannot_Open;
         end if;
         --  populate the unchanging fields in the Key_IE
         Key_IE.Time := IE_Time;
         Key_IE.IE_Type := EV_KEY;

         --  set up for keyboard events
         Request := IOW (UI_SET_EVBIT, 4);
         Result := K_IOCTL_3_Arg (Uinput_FID, Request, K_Long_T (EV_KEY));
         if Result = -1 then
            C_Perror ("UI_SET_EVBIT failed...");
            raise IOCTL_Error with "UI_SET_EVBIT failed";
         end if;

         --  enable all possible (0 .. 255) key values
         loop
            Result := K_IOCTL_3_Arg (Uinput_FID, IOW (UI_SET_KEYBIT, 4), K_Long_T (Key_Val));
            if Result = -1 then
               Ada.Text_IO.Put_Line ("ERROR: Could not SET_KEYBIT");
               raise IOCTL_Error with "Could not SET_KEYBIT";
            end if;
            Key_Val := Key_Val + 1;
            exit when Key_Val = 256;
         end loop;

         --  configure our dummy USB device...
         Usetup.ID.K_Bustype := BUS_USB;
         Usetup.ID.K_Vendor  := 16#1234#; --  sample vendor
         Usetup.ID.K_Product := 16#5678#; --  sample K_Product
         Usetup.Name (1 .. 9) := "Keypadder";
         Usetup.Name (10 .. Uinput_Max_Name_Size) := (others => Character'Val (0));
         Request := IOW (UI_DEV_SETUP, Usetup'Size / 8);
         Result := K_IOCTL_Uinput_Setup (Uinput_FID, Request, Usetup);
         if Result = -1 then
            C_Perror ("UI_DEV_SETUP failed...");
            raise IOCTL_Error with "UI_DEV_SETUP failed";
         end if;

         --  create the dummy device
         Request := IO (UI_DEV_CREATE, 0);
         Result := K_IOCTL_2_Arg (Uinput_FID, Request);
         if Result = -1 then
            C_Perror ("UI_DEV_CREATE failed...");
            raise IOCTL_Error with "UI_DEV_CREATE failed";
         end if;
      end Start;

      loop
         select
            accept Send (Keys : String) do
               Ada.Text_IO.Put_Line ("Injector got Send request for: " & Keys);
               for C in Keys'Range loop
                  --  Key press, report the event, send key release, and report again
                  Key_IE.IE_Code := Char_To_K_U16 (Keys (C));
                  Key_IE.IE_Value := 1;
                  --  IE_IO.Write (Uinput_File, Key_IE);
                  --  IE_IO.Write (Uinput_File, Report_IE);
                  Key_IE.IE_Value := 0;
                  --  IE_IO.Write (Uinput_File, Key_IE);
                  --  IE_IO.Write (Uinput_File, Report_IE);
               end loop;
            end Send;
         or
            accept Shutdown do
               K_Close (Result, Uinput_FID);
               return;
            end Shutdown;
         or
            terminate;
         end select;
      end loop;
   end Injector_Task;

end Injector;